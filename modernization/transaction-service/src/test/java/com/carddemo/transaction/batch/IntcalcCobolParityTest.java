package com.carddemo.transaction.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordReader;
import com.carddemo.common.record.RecordWriter;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.DisGroupRecordLayout;
import com.carddemo.common.record.layout.TranCatBalRecordLayout;
import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.CategoryBalanceId;
import com.carddemo.transaction.domain.DisclosureGroup;
import com.carddemo.transaction.domain.DisclosureGroupId;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DisclosureGroupRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Primary;

/**
 * INTCALC against CBACT04C itself. The expected SYSTRAN and ACCTDATA after-image under
 * {@code src/test/resources/intcalc/<set>/expected} were produced by
 * {@code scripts/migration/intcalc-reference/run.py}: CBACT04C compiled with GnuCOBOL, PARM
 * 2022071800, CURRENT-DATE frozen at 2022-07-18 00:00:00.00. The Java job runs on the same input
 * files with the clock frozen at the same instant; ACCTFILE and CXACAIX come from the same input files
 * through the stub gateways, and the recorded settlements are applied the way account-service does
 * (ADD to ACCT-CURR-BAL, cycle buckets to zero).
 */
@SpringBootTest
@Import(IntcalcCobolParityTest.FrozenClock.class)
class IntcalcCobolParityTest {

    private static final Path SETS = Path.of("src/test/resources/intcalc");

    @TempDir
    Path work;

    @Autowired
    private BatchJobLauncher launcher;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private CategoryBalanceRepository categoryBalances;

    @Autowired
    private DisclosureGroupRepository disclosureGroups;

    @Autowired
    private StubGateways.StubAccountGateway accounts;

    @Autowired
    private StubGateways.StubCardGateway cards;

    @ParameterizedTest(name = "{0}")
    @CsvSource({"sample, ../../app/data/ASCII", "edge, src/test/resources/intcalc/edge"})
    void systranAndAccountUpdatesAreByteIdenticalToCbact04c(String set, Path inputs) throws Exception {
        load(inputs);
        List<CobolRecord> accountRecords = read(inputs.resolve("acctdata.txt"), AccountRecordLayout.LAYOUT);
        accountRecords.forEach(r -> accounts.register(r.getLong(AccountRecordLayout.ACCT_ID),
                r.getTrimmed(AccountRecordLayout.ACCT_GROUP_ID), r.get(AccountRecordLayout.ACCT_CURR_BAL)));
        Path expected = SETS.resolve(set).resolve("expected");

        BatchRun run = launcher.launch(new BatchArguments("INTCALC", LocalDate.of(2022, 7, 18), "2022071800",
                work, Map.of(), null, null));

        assertThat(run.returnCode()).isZero();
        assertThat(Files.readString(expected.resolve("RETURN-CODE")).strip()).isEqualTo("0");
        assertThat(Files.readAllBytes(run.outputDir().resolve("SYSTRAN")))
                .isEqualTo(Files.readAllBytes(expected.resolve("SYSTRAN")));
        byte[] actualAccounts = RecordWriter.toBytes(settle(accountRecords), RecordFormat.FIXED);
        assertThat(actualAccounts)
                .isEqualTo(Files.readAllBytes(expected.resolve("ACCTDATA")));
    }

    private void load(Path inputs) throws IOException {
        transactions.deleteAll();
        categoryBalances.deleteAll();
        disclosureGroups.deleteAll();
        for (CobolRecord r : read(inputs.resolve("tcatbal.txt"), TranCatBalRecordLayout.LAYOUT)) {
            categoryBalances.save(new CategoryBalance(new CategoryBalanceId(
                    r.getLong(TranCatBalRecordLayout.TRANCAT_ACCT_ID), r.get(TranCatBalRecordLayout.TRANCAT_TYPE_CD),
                    r.getInt(TranCatBalRecordLayout.TRANCAT_CD)), r.get(TranCatBalRecordLayout.TRAN_CAT_BAL)));
        }
        for (CobolRecord r : read(inputs.resolve("discgrp.txt"), DisGroupRecordLayout.LAYOUT)) {
            disclosureGroups.save(new DisclosureGroup(new DisclosureGroupId(
                    r.getTrimmed(DisGroupRecordLayout.DIS_ACCT_GROUP_ID), r.get(DisGroupRecordLayout.DIS_TRAN_TYPE_CD),
                    r.getInt(DisGroupRecordLayout.DIS_TRAN_CAT_CD)), r.get(DisGroupRecordLayout.DIS_INT_RATE)));
        }
        accounts.clear();
        cards.clear();
        for (CobolRecord r : read(inputs.resolve("cardxref.txt"), CardXrefRecordLayout.LAYOUT)) {
            cards.register(r.get(CardXrefRecordLayout.XREF_CARD_NUM), r.getLong(CardXrefRecordLayout.XREF_CUST_ID),
                    r.getLong(CardXrefRecordLayout.XREF_ACCT_ID));
        }
    }

    /** 1050-UPDATE-ACCOUNT as account-service applies it: ADD to ACCT-CURR-BAL, cycle buckets to zero. */
    private List<CobolRecord> settle(List<CobolRecord> records) {
        for (CobolRecord r : records) {
            BigDecimal interest = accounts.settledByAccount().get(r.getLong(AccountRecordLayout.ACCT_ID));
            if (interest != null) {
                r.set(AccountRecordLayout.ACCT_CURR_BAL, r.get(AccountRecordLayout.ACCT_CURR_BAL).add(interest));
                r.set(AccountRecordLayout.ACCT_CURR_CYC_CREDIT, BigDecimal.ZERO);
                r.set(AccountRecordLayout.ACCT_CURR_CYC_DEBIT, BigDecimal.ZERO);
            }
        }
        return records;
    }

    private static List<CobolRecord> read(Path file, com.carddemo.common.record.RecordLayout layout)
            throws IOException {
        return RecordReader.readAll(file, layout, CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);
    }

    /** Imported only here: unannotated, so component scanning in other tests never picks it up. */
    static class FrozenClock {

        @Bean
        @Primary
        Clock frozenClock() {
            return Clock.fixed(Instant.parse("2022-07-18T00:00:00Z"), ZoneOffset.UTC);
        }
    }
}
