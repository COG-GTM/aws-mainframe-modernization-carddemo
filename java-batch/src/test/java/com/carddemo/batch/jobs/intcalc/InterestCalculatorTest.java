package com.carddemo.batch.jobs.intcalc;

import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.DisclosureGroupRecord;
import com.carddemo.batch.model.DisclosureGroupRecord.DisGroupKey;
import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TranCatBalRecord.TranCatKey;
import com.carddemo.batch.model.TransactionRecord;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class InterestCalculatorTest {

    private KsdsFile<Long, AccountRecord> accounts;
    private KsdsFile<String, CardXrefRecord> xrefs;
    private KsdsFile<DisGroupKey, DisclosureGroupRecord> groups;
    private InterestCalculator calc;

    @BeforeEach
    void setUp() {
        accounts = new KsdsFile<>(AccountRecord.CODEC, AccountRecord::acctId);
        xrefs = new KsdsFile<>(CardXrefRecord.CODEC, CardXrefRecord::xrefCardNum);
        groups = new KsdsFile<>(DisclosureGroupRecord.CODEC, DisclosureGroupRecord::disGroupKey);
        accounts.write(account(1, "GOLD", "100.00"));
        accounts.write(account(2, "", "-50.00"));
        accounts.write(account(3, "", "0.00"));
        xrefs.write(new CardXrefRecord("4000000000000003", 3, 3, ""));
        xrefs.write(new CardXrefRecord("4000000000000002", 2, 2, ""));
        xrefs.write(new CardXrefRecord("4000000000000001", 1, 1, ""));
        groups.write(group("GOLD", "01", 1, "18.99"));
        groups.write(group("GOLD", "01", 2, "0.00"));
        groups.write(group("DEFAULT", "01", 1, "25.00"));
        calc = new InterestCalculator(accounts, xrefs, groups, "2022071800",
                Clock.fixed(Instant.parse("2022-07-18T01:02:03.456Z"), ZoneOffset.UTC));
    }

    @Test
    void truncatesMonthlyInterestLikeCobolCompute() {
        // 1234.56 * 18.99 / 1200 = 19.5369... -> 19.53 (no ROUNDED)
        TransactionRecord tx = calc.process(tcatBal(1, 1, "1234.56")).orElseThrow();
        assertThat(tx.tranAmt()).isEqualByComparingTo("19.53");
        assertThat(tx.tranId()).isEqualTo("2022071800000001");
        assertThat(tx.tranDesc()).isEqualTo("Int. for a/c 00000000001");
        assertThat(tx.tranCardNum()).isEqualTo("4000000000000001");
        assertThat(tx.tranOrigTs()).isEqualTo("2022-07-18-01.02.03.450000");
    }

    @Test
    void negativeBalanceTruncatesTowardZeroAndDefaultGroupIsUsed() {
        // account 2 has a blank group id: DEFAULT rate 25.00; -10.01 * 25 / 1200 = -0.2085... -> -0.20
        TransactionRecord tx = calc.process(tcatBal(2, 1, "-10.01")).orElseThrow();
        assertThat(tx.tranAmt()).isEqualByComparingTo("-0.20");
        calc.process(tcatBal(3, 1, "0.00"));
        assertThat(accounts.read(2L).orElseThrow().acctCurrBal()).isEqualByComparingTo("-50.20");
    }

    @Test
    void zeroRateWritesNoTransactionButStillResetsCycleTotals() {
        assertThat(calc.process(tcatBal(1, 2, "500.00"))).isEmpty();
        calc.process(tcatBal(2, 1, "0.00"));
        AccountRecord a = accounts.read(1L).orElseThrow();
        assertThat(a.acctCurrBal()).isEqualByComparingTo("100.00");
        assertThat(a.acctCurrCycCredit()).isEqualByComparingTo("0");
        assertThat(a.acctCurrCycDebit()).isEqualByComparingTo("0");
    }

    @Test
    void accountIsRewrittenOnBreakWithTotalInterest() {
        calc.process(tcatBal(1, 1, "1200.00"));   // 18.99
        calc.process(tcatBal(1, 1, "600.00"));    // 9.49
        calc.process(tcatBal(2, 1, "0.00"));      // break: account 1 rewritten
        assertThat(accounts.read(1L).orElseThrow().acctCurrBal()).isEqualByComparingTo("128.48");
        assertThat(calc.transactionCount()).isEqualTo(3);
    }

    @Test
    void lastAccountIsNotRewrittenAtEndOfFileLikeTheCobol() {
        calc.process(tcatBal(1, 1, "1200.00"));
        calc.finish();
        AccountRecord a = accounts.read(1L).orElseThrow();
        assertThat(a.acctCurrBal()).isEqualByComparingTo("100.00");
        assertThat(a.acctCurrCycCredit()).isEqualByComparingTo("11.11");
    }

    @Test
    void missingDefaultGroupAbends() {
        assertThatThrownBy(() -> calc.process(tcatBal(2, 9, "1.00"))).isInstanceOf(IntcalcAbendException.class);
    }

    @Test
    void fitTruncatesHighOrderDigits() {
        assertThat(InterestCalculator.fit(new BigDecimal("1234567890.129"), 9, 2)).isEqualByComparingTo("234567890.12");
    }

    private static AccountRecord account(long id, String group, String balance) {
        BigDecimal z = BigDecimal.ZERO.setScale(2);
        return new AccountRecord(id, "Y", new BigDecimal(balance), z, z, "2020-01-01", "2030-01-01", "2030-01-01",
                new BigDecimal("11.11"), new BigDecimal("22.22"), "", group, "");
    }

    private static DisclosureGroupRecord group(String id, String type, int cat, String rate) {
        return new DisclosureGroupRecord(new DisGroupKey(id, type, cat), new BigDecimal(rate), "");
    }

    private static TranCatBalRecord tcatBal(long acct, int cat, String bal) {
        return new TranCatBalRecord(new TranCatKey(acct, "01", cat), new BigDecimal(bal), "");
    }
}
