package com.carddemo.app.data;

import com.carddemo.app.PostgresContainerSupport;
import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.ParityHarness;
import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.RecordFiles;
import com.carddemo.reference.domain.TranCatBalance;
import com.carddemo.reference.domain.TranCatBalanceId;
import com.carddemo.reference.repository.TranCatBalanceRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** Real persistence on PostgreSQL 16 + Flyway V2: copybook records survive load → table → unload unchanged. */
@SpringBootTest
class KsdsDataServiceTest extends PostgresContainerSupport {

    @Autowired
    KsdsDataService data;
    @Autowired
    AccountRepository accounts;
    @Autowired
    TranCatBalanceRepository balances;

    @BeforeEach
    void empty() {
        data.deleteAll();
    }

    @Test
    void sampleDataLoadThenUnloadEqualsTheSourceFiles() {
        data.loadSampleData(GoldenCase.SAMPLE_DATA);
        for (Ksds ksds : List.of(Ksds.ACCTDATA, Ksds.CARDXREF, Ksds.TCATBALF)) {
            List<String> source = RecordFiles.readLines(GoldenCase.SAMPLE_DATA.resolve(ksds.sampleFile()),
                    ksds.recordLength(), CobolCharsets.ASCII);
            assertThat(data.unload(ksds)).as(ksds.name()).hasSize(50).isEqualTo(source);
        }
        assertThat(data.unload(Ksds.TRANSACT)).isEmpty();
    }

    @Test
    void everyOracleEndStateRoundTripsThroughTheTables() {
        for (GoldenCase c : GoldenCase.all()) {
            data.deleteAll();
            for (Ksds ksds : List.of(Ksds.ACCTDATA, Ksds.TCATBALF, Ksds.TRANSACT)) {
                data.load(ksds, RecordFiles.readLines(c.expectedUnload(ksds), ksds.recordLength(), CobolCharsets.ASCII));
                ParityHarness.assertTable(c, ksds, data.unload(ksds));
            }
        }
    }

    @Test
    void unloadIsInKsdsKeyOrderWhateverTheInsertOrder() {
        GoldenCase c = GoldenCase.named("baseline");
        for (Ksds ksds : List.of(Ksds.TCATBALF, Ksds.TRANSACT)) {
            List<String> shuffled = new ArrayList<>(RecordFiles.readLines(c.expectedUnload(ksds), ksds.recordLength(), CobolCharsets.ASCII));
            Collections.reverse(shuffled);
            data.load(ksds, shuffled);
            ParityHarness.assertTable(c, ksds, data.unload(ksds));
        }
    }

    @Test
    void duplicateKeyFailsLikeKsdsStatus22() {
        data.loadSampleData(GoldenCase.SAMPLE_DATA);
        List<String> again = RecordFiles.readLines(GoldenCase.SAMPLE_DATA.resolve("acctdata.txt"), 300, CobolCharsets.ASCII).subList(0, 1);
        assertThatThrownBy(() -> data.load(Ksds.ACCTDATA, again)).isInstanceOf(DataIntegrityViolationException.class);
    }

    @Test
    void moneyIsExactDecimalAndRewrittenMastersAreVersioned() {
        data.loadSampleData(GoldenCase.SAMPLE_DATA);
        Account first = accounts.findById(1L).orElseThrow();
        assertThat(first.getCurrBal()).isEqualTo(new BigDecimal("194.00"));
        long v0 = first.getVersion();
        first.setCurrBal(first.getCurrBal().add(new BigDecimal("-0.01")));
        Account saved = accounts.saveAndFlush(first);
        assertThat(saved.getVersion()).isEqualTo(v0 + 1);
        assertThat(accounts.findById(1L).orElseThrow().getCurrBal()).isEqualTo(new BigDecimal("193.99"));

        TranCatBalance stale = balances.findById(new TranCatBalanceId(1L, "01", 1)).orElseThrow();
        TranCatBalance fresh = balances.findById(new TranCatBalanceId(1L, "01", 1)).orElseThrow();
        fresh.setBalance(new BigDecimal("1.00"));
        balances.saveAndFlush(fresh);
        stale.setBalance(new BigDecimal("2.00"));
        assertThatThrownBy(() -> balances.saveAndFlush(stale)).isInstanceOf(ObjectOptimisticLockingFailureException.class);
    }

    @Test
    void negativeZeroInEverySignedFieldSurvivesLoadAndUnload() {
        String account = RecordFiles.readLines(GoldenCase.SAMPLE_DATA.resolve("acctdata.txt"), 300, CobolCharsets.ASCII).get(0);
        com.carddemo.common.record.FixedRecord a = com.carddemo.common.record.Copybooks.CVACT01Y.parse(account);
        for (String f : List.of("ACCT-CURR-BAL", "ACCT-CREDIT-LIMIT", "ACCT-CASH-CREDIT-LIMIT", "ACCT-CURR-CYC-CREDIT", "ACCT-CURR-CYC-DEBIT")) {
            a.set(f, BigDecimal.ZERO, true);
        }
        String tcatbal = com.carddemo.common.record.Copybooks.CVTRA01Y.blank().set("TRANCAT-ACCT-ID", 1).set("TRANCAT-TYPE-CD", "01")
                .set("TRANCAT-CD", 1).set("TRAN-CAT-BAL", BigDecimal.ZERO, true).toString();
        String tran = RecordFiles.readLines(GoldenCase.named("syn-ok").expected("TRANSACT.unl"), 350, CobolCharsets.ASCII).get(0);
        String negTran = com.carddemo.common.record.Copybooks.CVTRA05Y.parse(tran).set("TRAN-AMT", BigDecimal.ZERO, true).toString();
        data.load(Ksds.ACCTDATA, List.of(a.toString()));
        data.load(Ksds.TCATBALF, List.of(tcatbal));
        data.load(Ksds.TRANSACT, List.of(negTran));
        assertThat(data.unload(Ksds.ACCTDATA)).containsExactly(a.toString());
        assertThat(data.unload(Ksds.TCATBALF)).containsExactly(tcatbal);
        assertThat(data.unload(Ksds.TRANSACT)).containsExactly(negTran);
        assertThat(a.toString()).contains("00000000000}");
    }
}
