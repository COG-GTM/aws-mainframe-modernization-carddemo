package com.carddemo.interest.domain;

import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.AccountLayout;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.CardXrefLayout;
import com.carddemo.recordio.layout.DisclosureGroup;
import com.carddemo.recordio.layout.DisclosureGroupLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.recordio.store.RecordNotFoundException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** CBACT04C main loop + 1050-UPDATE-ACCOUNT + 1300-COMPUTE-INTEREST (lines 197-235, 300-320, 386-434). */
class InterestPostingRunTest {

    private static final RecordEncoding ENC = RecordEncoding.EBCDIC;
    private static final String A1 = "00000000001";
    private static final String A2 = "00000000002";
    private KeyedRecordStore<Account> accounts;
    private InterestPostingRun run;

    @BeforeEach
    void setUp() {
        accounts = KeyedRecordStore.of("ACCTFILE", List.of(
                new Account(A1, "Y", new BigDecimal("1000.00"), new BigDecimal("5000.00"), new BigDecimal("1000.00"),
                        "2020-01-01", "2030-01-01", "2025-01-01", new BigDecimal("700.00"), new BigDecimal("-50.00"), "1", "GOLD"),
                new Account(A2, "Y", new BigDecimal("0.00"), new BigDecimal("5000.00"), new BigDecimal("1000.00"),
                        "2020-01-01", "2030-01-01", "2025-01-01", BigDecimal.ZERO, BigDecimal.ZERO, "1", "")),
                AccountLayout.INSTANCE, ENC, Account::accountId);
        var xref = KeyedRecordStore.of("XREFFILE", List.of(
                new CardXref("4000000000000009", 1L, A1),
                new CardXref("4000000000000001", 1L, A1),     // lower card number, same account
                new CardXref("4000000000000002", 2L, A2)),
                CardXrefLayout.INSTANCE, ENC, CardXref::cardNumber);
        var rates = new InterestRateLookup(KeyedRecordStore.of("DISCGRP", List.of(
                new DisclosureGroup("GOLD", "01", 1, new BigDecimal("12.00")),
                new DisclosureGroup("DEFAULT", "01", 1, new BigDecimal("24.00")),
                new DisclosureGroup("DEFAULT", "01", 2, new BigDecimal("12.00"))),
                DisclosureGroupLayout.INSTANCE, ENC, DisclosureGroup::key));
        run = new InterestPostingRun(accounts, xref, rates,
                new InterestTransactionFactory("2022071800", Clock.fixed(Instant.parse("2022-07-18T06:00:00Z"), ZoneOffset.UTC)));
    }

    private static TransactionCategoryBalance bal(String acct, int cat, String amount) {
        return new TransactionCategoryBalance(acct, "01", cat, new BigDecimal(amount));
    }

    @Test
    void controlBreakAppliesTotalInterestAndResetsCycleBuckets() {
        run.accept(bal(A1, 1, "1200.00"));   // GOLD 12% -> 12.00
        run.accept(bal(A1, 2, "600.00"));    // no GOLD row for cat 2 -> DEFAULT 12% -> 6.00
        run.accept(bal(A2, 1, "100.00"));    // blank group -> DEFAULT 24% -> 2.00
        run.finish();

        Account a1 = accounts.read(A1).orElseThrow();
        assertThat(a1.currentBalance()).isEqualByComparingTo("1018.00");
        assertThat(a1.currentCycleCredit()).isZero();
        assertThat(a1.currentCycleDebit()).isZero();
        assertThat(accounts.read(A2).orElseThrow().currentBalance()).isEqualByComparingTo("2.00");
    }

    @Test
    void oneSystemTransactionPerCategoryBalanceWithParmDateIdsAndFirstCardOfAccount() {
        run.accept(bal(A1, 1, "1200.00"));
        run.accept(bal(A1, 2, "600.00"));
        run.accept(bal(A2, 1, "100.00"));
        run.finish();

        List<Transaction> out = run.systemTransactions();
        assertThat(out).extracting(Transaction::id).containsExactly("2022071800000001", "2022071800000002", "2022071800000003");
        assertThat(out).extracting(Transaction::amount).containsExactly(new BigDecimal("12.00"), new BigDecimal("6.00"), new BigDecimal("2.00"));
        Transaction first = out.get(0);
        assertThat(first.typeCode()).isEqualTo("01");
        assertThat(first.categoryCode()).isEqualTo(5);
        assertThat(first.source()).isEqualTo("System");
        assertThat(first.description()).isEqualTo("Int. for a/c " + A1);
        assertThat(first.merchantId()).isZero();
        assertThat(first.cardNumber()).isEqualTo("4000000000000001");
        assertThat(first.originalTimestamp()).isEqualTo("2022-07-18-06.00.00.000000");
        assertThat(first.processingTimestamp()).isEqualTo(first.originalTimestamp());
        assertThat(TransactionLayout.INSTANCE.encode(first, ENC).length()).isEqualTo(350);
    }

    @Test
    void zeroInterestStillWritesATransaction() {
        run.accept(bal(A2, 1, "0.00"));
        run.finish();
        assertThat(run.systemTransactions()).hasSize(1);
        assertThat(run.systemTransactions().get(0).amount()).isZero();
    }

    @Test
    void unknownAccountIsFatal() {
        assertThatThrownBy(() -> run.accept(bal("00000000099", 1, "1.00")))
                .isInstanceOf(RecordNotFoundException.class);
    }

    @Test
    void emptyInputProducesNoOutputAndTouchesNoAccount() {
        run.finish();
        assertThat(run.systemTransactions()).isEmpty();
        assertThat(accounts.read(A1).orElseThrow().currentCycleCredit()).isEqualByComparingTo("700.00");
    }
}
