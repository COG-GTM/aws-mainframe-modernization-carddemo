package com.carddemo.interest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.interest.model.AccountRecord;
import com.carddemo.interest.model.CardXrefRecord;
import com.carddemo.interest.model.DisclosureGroupKey;
import com.carddemo.interest.model.DisclosureGroupRecord;
import com.carddemo.interest.model.TranCatBalRecord;
import com.carddemo.interest.model.TransactionRecord;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;

/** Control-break behaviour of the CBACT04C PROCEDURE DIVISION loop, driven by in-memory files. */
class InterestCalculationBatchTest {

    private static final LocalDateTime FIXED_CLOCK = LocalDateTime.of(2022, 7, 18, 12, 0, 0);
    private static final String RUN_DATE = "2022071800";

    private final Map<String, AccountRecord> accountFile = new LinkedHashMap<>();
    private final Map<String, CardXrefRecord> xrefFile = new HashMap<>();
    private final Map<DisclosureGroupKey, DisclosureGroupRecord> discgrpFile = new HashMap<>();
    private final List<TransactionRecord> writtenTransactions = new ArrayList<>();

    private final InterestCalculationBatch.AccountStore accounts = new InterestCalculationBatch.AccountStore() {
        @Override
        public Optional<AccountRecord> read(String acctId) {
            return Optional.ofNullable(accountFile.get(acctId));
        }

        @Override
        public void rewrite(AccountRecord account) {
            accountFile.put(account.acctId(), account);
        }
    };

    private final InterestCalculationBatch.CardXrefStore xrefs =
            acctId -> Optional.ofNullable(xrefFile.get(acctId));

    private final InterestCalculationBatch.DisclosureGroupStore disclosureGroups =
            key -> Optional.ofNullable(discgrpFile.get(key));

    private final InterestCalculationBatch.TransactionWriter transactions = writtenTransactions::add;

    private void givenAccount(String acctId, String balance, String groupId) {
        accountFile.put(CobolDecimal.zoned(acctId, 11), new AccountRecord(
                acctId, new BigDecimal(balance), new BigDecimal("50.00"), new BigDecimal("70.00"), groupId));
        xrefFile.put(CobolDecimal.zoned(acctId, 11),
                new CardXrefRecord("4111" + CobolDecimal.zoned(acctId, 12), "1", acctId));
    }

    private void givenRate(String groupId, String typeCd, String catCd, String rate) {
        DisclosureGroupRecord record = new DisclosureGroupRecord(groupId, typeCd, catCd, new BigDecimal(rate));
        discgrpFile.put(record.key(), record);
    }

    private InterestCalculationBatch batch(List<TranCatBalRecord> input, boolean postFinalAccountGroup) {
        InterestCalculationBatch.TranCatBalReader reader = new InterestCalculationBatch.TranCatBalReader() {
            @Override
            public Iterator<TranCatBalRecord> iterator() {
                return input.iterator();
            }
        };
        return new InterestCalculationBatch(
                reader, accounts, xrefs, disclosureGroups, transactions, () -> FIXED_CLOCK, postFinalAccountGroup);
    }

    @Test
    void accumulatesEveryCategoryOfAnAccountBeforePostingToTheBalance() {
        givenAccount("1", "100.00", "GROUP01");
        givenAccount("2", "200.00", "GROUP01");
        givenRate("GROUP01", "01", "0001", "12.00");
        givenRate("GROUP01", "02", "0002", "6.00");

        InterestCalculationBatch.Result result = batch(List.of(
                new TranCatBalRecord("1", "01", "0001", new BigDecimal("1200.00")),
                new TranCatBalRecord("1", "02", "0002", new BigDecimal("1200.00")),
                new TranCatBalRecord("2", "01", "0001", new BigDecimal("1200.00"))), true).run(RUN_DATE);

        assertEquals(new InterestCalculationBatch.Result(3, 3, 2), result);
        // account 1: 12.00 + 6.00 posted once at the control break
        assertEquals(new BigDecimal("118.00"), accountFile.get("00000000001").currentBalance());
        assertEquals(new BigDecimal("212.00"), accountFile.get("00000000002").currentBalance());
        assertEquals(List.of("2022071800000001", "2022071800000002", "2022071800000003"),
                writtenTransactions.stream().map(TransactionRecord::tranId).toList());
    }

    @Test
    void skipsInterestAndTransactionWhenTheDisclosureRateIsZero() {
        givenAccount("1", "100.00", "ZEROAPR");
        givenRate("ZEROAPR", "01", "0001", "0.00");

        InterestCalculationBatch.Result result = batch(List.of(
                new TranCatBalRecord("1", "01", "0001", new BigDecimal("5000.00"))), true).run(RUN_DATE);

        assertEquals(new InterestCalculationBatch.Result(1, 0, 1), result);
        assertTrue(writtenTransactions.isEmpty());
        // The account is still rewritten, which zeroes the cycle credit and debit totals.
        assertEquals(new BigDecimal("100.00"), accountFile.get("00000000001").currentBalance());
        assertEquals(0, accountFile.get("00000000001").currentCycleDebit().compareTo(BigDecimal.ZERO));
    }

    @Test
    void fallsBackToTheDefaultDisclosureGroupWhenTheAccountGroupHasNoRecord() {
        givenAccount("1", "100.00", "NOSUCHGRP");
        givenRate("DEFAULT", "01", "0001", "12.00");

        batch(List.of(new TranCatBalRecord("1", "01", "0001", new BigDecimal("1200.00"))), true).run(RUN_DATE);

        assertEquals(new BigDecimal("112.00"), accountFile.get("00000000001").currentBalance());
    }

    @Test
    void abendsWhenNeitherTheAccountGroupNorTheDefaultGroupHasARate() {
        givenAccount("1", "100.00", "NOSUCHGRP");

        InterestCalculationBatch run = batch(
                List.of(new TranCatBalRecord("1", "01", "0001", new BigDecimal("1200.00"))), true);

        assertThrows(IllegalStateException.class, () -> run.run(RUN_DATE));
    }

    @Test
    void abendsWhenTheAccountIsMissingFromTheAccountFile() {
        givenRate("GROUP01", "01", "0001", "12.00");

        InterestCalculationBatch run = batch(
                List.of(new TranCatBalRecord("1", "01", "0001", new BigDecimal("1200.00"))), true);

        assertThrows(IllegalStateException.class, () -> run.run(RUN_DATE));
    }

    @Test
    void zeroBalanceCategoryWritesAZeroAmountInterestTransaction() {
        givenAccount("1", "100.00", "GROUP01");
        givenRate("GROUP01", "01", "0001", "12.00");

        batch(List.of(new TranCatBalRecord("1", "01", "0001", new BigDecimal("0.00"))), true).run(RUN_DATE);

        assertEquals(1, writtenTransactions.size());
        assertEquals(0, writtenTransactions.get(0).amount().compareTo(BigDecimal.ZERO));
        assertEquals(new BigDecimal("100.00"), accountFile.get("00000000001").currentBalance());
    }

    @Test
    void legacyLoopNeverPostsInterestForTheLastAccountInTheFile() {
        givenAccount("1", "100.00", "GROUP01");
        givenAccount("2", "200.00", "GROUP01");
        givenRate("GROUP01", "01", "0001", "12.00");

        InterestCalculationBatch.Result result = batch(List.of(
                new TranCatBalRecord("1", "01", "0001", new BigDecimal("1200.00")),
                new TranCatBalRecord("2", "01", "0001", new BigDecimal("1200.00"))), false).run(RUN_DATE);

        assertEquals(new InterestCalculationBatch.Result(2, 2, 1), result);
        assertEquals(new BigDecimal("112.00"), accountFile.get("00000000001").currentBalance());
        // Account 2 got an interest transaction but no balance update: the 1050-UPDATE-ACCOUNT call
        // for the final control break sits in an unreachable ELSE branch of the PERFORM loop.
        assertEquals(new BigDecimal("200.00"), accountFile.get("00000000002").currentBalance());
        assertEquals(2, writtenTransactions.size());
    }
}
