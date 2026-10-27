package com.carddemo.modernization.interest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.CardXrefRecord;
import com.carddemo.modernization.records.DisclosureGroupRecord;
import com.carddemo.modernization.records.TranCatBalanceRecord;
import com.carddemo.modernization.records.TransactionRecord;
import com.carddemo.modernization.records.codec.TextCodec;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import com.carddemo.modernization.records.codec.ZonedDecimalCodec;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.Test;

class InterestCalculatorTest {
    private static final String PARM_DATE = "2022071800";
    private static final Clock FIXED_CLOCK =
            Clock.fixed(Instant.parse("2022-07-18T12:34:56.123456789Z"), ZoneOffset.UTC);

    @Test
    void int01RequiresA10CharacterParmDate() {
        assertThrows(IllegalArgumentException.class, () -> new InterestCalculator(FIXED_CLOCK, "202207180"));
        assertThrows(IllegalArgumentException.class, () -> new InterestCalculator(FIXED_CLOCK, "20220718000"));
        assertThrows(IllegalArgumentException.class, () -> new InterestCalculator(FIXED_CLOCK, null));
        new InterestCalculator(FIXED_CLOCK, PARM_DATE);
    }

    @Test
    void int03SortsAndDisplaysCategoryBalancesAndRejectsDuplicatePrimaryKeys() {
        TranCatBalanceRecord first = category("00000000001", 1, "100.00");
        TranCatBalanceRecord second = category("00000000001", 2, "200.00");
        InterestRun run = calculator().run(input(
                List.of(second, first),
                List.of(account("00000000001", "GROUP", "0.00", "1.00", "2.00")),
                List.of(xref("00000000001", "0000000000000001")),
                List.of(disclosure("GROUP", 1, "12.00"), disclosure("GROUP", 2, "12.00"))));
        assertEquals(List.of(
                "START OF EXECUTION OF PROGRAM CBACT04C",
                display(first),
                display(second),
                "END OF EXECUTION OF PROGRAM CBACT04C"), run.sysout());
        assertEquals(PARM_DATE + "000001", run.transactions().get(0).id());
        assertEquals(PARM_DATE + "000002", run.transactions().get(1).id());

        assertThrows(IllegalArgumentException.class, () -> calculator().run(input(
                List.of(first, first), List.of(), List.of(), List.of())));
        assertThrows(IllegalArgumentException.class, () -> calculator().run(input(
                List.of(), List.of(
                        account("00000000001", "G", "0.00", "0.00", "0.00"),
                        account("00000000001", "G", "0.00", "0.00", "0.00")),
                List.of(), List.of())));
        CardXrefRecord sameCard = xref("00000000001", "0000000000000001");
        assertThrows(IllegalArgumentException.class, () -> calculator().run(input(
                List.of(), List.of(), List.of(sameCard, sameCard), List.of())));
        assertThrows(IllegalArgumentException.class, () -> calculator().run(input(
                List.of(), List.of(), List.of(
                        xref("00000000001", "1"), xref("00000000002", "1 ")), List.of())));
        assertThrows(IllegalArgumentException.class, () -> calculator().run(input(
                List.of(), List.of(), List.of(),
                List.of(disclosure("G", 1, "1.00"), disclosure("G         ", 1, "2.00")))));
    }

    @Test
    void int07ChoosesTheFirstCrossReferenceInCardNumberPrimaryKeyOrder() {
        InterestRun run = calculator().run(input(
                List.of(category("00000000001", 1, "100.00")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                List.of(
                        xref("00000000001", "0000000000000002"),
                        xref("00000000001", "0000000000000001")),
                List.of(disclosure("GROUP", 1, "12.00"))));
        assertEquals("0000000000000001", run.transactions().getFirst().cardNumber());
    }

    @Test
    void int05RewritesThePreviousAccountButNotTheLastAccount() {
        AccountRecord first = account("00000000001", "GROUP", "50.00", "3.00", "2.00");
        AccountRecord last = account("00000000002", "GROUP", "60.00", "4.00", "1.00");
        InterestRun run = calculator().run(input(
                List.of(category(first.acctId(), 1, "100.00"), category(last.acctId(), 1, "100.00")),
                List.of(last, first),
                List.of(xref(first.acctId(), "0000000000000001"), xref(last.acctId(), "0000000000000002")),
                List.of(disclosure("GROUP", 1, "12.00"))));

        AccountRecord rewrittenFirst = run.accounts().get(0);
        assertEquals("00000000001", rewrittenFirst.acctId());
        assertEquals(new BigDecimal("51.00"), rewrittenFirst.currentBalance().value());
        assertEquals(new BigDecimal("0.00"), rewrittenFirst.cycleCredit().value());
        assertEquals(new BigDecimal("0.00"), rewrittenFirst.cycleDebit().value());
        assertEquals(last, run.accounts().get(1));
        assertEquals(2, run.transactions().size());
    }

    @Test
    void int09UsesPrimaryRateThenDefaultAndAbendsWhenDefaultIsMissing() {
        InterestInput primaryInput = input(
                List.of(category("00000000001", 1, "100.00")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                List.of(xref("00000000001", "0000000000000001")),
                List.of(disclosure("GROUP", 1, "12.00"), disclosure("DEFAULT", 1, "24.00")));
        InterestRun primaryRun = calculator().run(primaryInput);
        assertFalse(primaryRun.sysout().contains("DISCLOSURE GROUP RECORD MISSING"));
        assertEquals(new BigDecimal("1.00"), primaryRun.transactions().get(0).amount().value());

        InterestInput fallbackInput = input(
                List.of(category("00000000001", 1, "100.00")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                List.of(xref("00000000001", "0000000000000001")),
                List.of(disclosure("DEFAULT", 1, "24.00")));
        InterestRun fallbackRun = calculator().run(fallbackInput);
        assertEquals(List.of(
                "DISCLOSURE GROUP RECORD MISSING", "TRY WITH DEFAULT GROUP CODE"),
                fallbackRun.sysout().subList(2, 4));
        assertEquals(new BigDecimal("2.00"), fallbackRun.transactions().get(0).amount().value());

        InterestAbendException missingDefault =
                assertThrows(InterestAbendException.class, () -> calculator().run(input(
                        List.of(category("00000000001", 1, "100.00")),
                        List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                        List.of(xref("00000000001", "0000000000000001")),
                        List.of())));
        assertEquals(List.of("ERROR READING DEFAULT DISCLOSURE GROUP"), missingDefault.displayLines());
        assertEquals("23", missingDefault.fileStatus());
    }

    @Test
    void int06AndInt07AbendForMissingAccountAndCrossReference() {
        InterestAbendException missingAccount = assertThrows(InterestAbendException.class, () ->
                calculator().run(input(
                        List.of(category("00000000001", 1, "100.00")), List.of(), List.of(), List.of())));
        assertEquals(List.of("ACCOUNT NOT FOUND: 00000000001", "ERROR READING ACCOUNT FILE"),
                missingAccount.displayLines());
        assertEquals("23", missingAccount.fileStatus());

        InterestAbendException missingXref = assertThrows(InterestAbendException.class, () ->
                calculator().run(input(
                        List.of(category("00000000001", 1, "100.00")),
                        List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                        List.of(),
                        List.of())));
        assertEquals(List.of("ACCOUNT NOT FOUND: 00000000001", "ERROR READING XREF FILE"),
                missingXref.displayLines());
        assertEquals("23", missingXref.fileStatus());
    }

    @Test
    void int10ZeroRateWritesNoTransactionButRewritesOnTheNextAccountBreak() {
        AccountRecord zeroRateAccount = account("00000000001", "GROUP", "20.00", "8.00", "9.00");
        AccountRecord lastAccount = account("00000000002", "GROUP", "30.00", "7.00", "6.00");
        InterestRun run = calculator().run(input(
                List.of(category(zeroRateAccount.acctId(), 1, "100.00"),
                        category(lastAccount.acctId(), 1, "100.00")),
                List.of(zeroRateAccount, lastAccount),
                List.of(xref(zeroRateAccount.acctId(), "0000000000000001"),
                        xref(lastAccount.acctId(), "0000000000000002")),
                List.of(disclosure("GROUP", 1, "0.00"))));
        assertTrue(run.transactions().isEmpty());
        assertEquals(new BigDecimal("20.00"), run.accounts().get(0).currentBalance().value());
        assertEquals(new BigDecimal("0.00"), run.accounts().get(0).cycleCredit().value());
        assertEquals(new BigDecimal("0.00"), run.accounts().get(0).cycleDebit().value());
    }

    @Test
    void int11TruncatesMonthlyInterestAndEncodesZeroAsPositive() {
        InterestRun run = calculator().run(input(
                List.of(
                        category("00000000001", 1, "1000.00"),
                        category("00000000001", 2, "-100.01"),
                        category("00000000001", 3, "-0.19")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                List.of(xref("00000000001", "0000000000000001")),
                List.of(
                        disclosure("GROUP", 1, "18.99"),
                        disclosure("GROUP", 2, "12.50"),
                        disclosure("GROUP", 3, "12.50"))));

        assertEquals(new BigDecimal("15.82"), run.transactions().get(0).amount().value());
        assertEquals(new BigDecimal("-1.04"), run.transactions().get(1).amount().value());
        assertEquals(new BigDecimal("0.00"), run.transactions().get(2).amount().value());
        byte[] zero = new ZonedDecimalCodec(11, 2).encode(run.transactions().get(2).amount(), TextCodec.ASCII);
        assertEquals(11, zero.length);
        assertEquals("0000000000{", new String(zero, StandardCharsets.US_ASCII));
        assertFalse(run.transactions().get(2).amount().negativeZero());
    }

    @Test
    void int11TruncateToPicDropsHighOrderDigitsAndKeepsNegativeSign() {
        assertEquals(new BigDecimal("345678901.23"),
                InterestCalculator.truncateToPic(new BigDecimal("12345678901.23"), 9));
        assertEquals(new BigDecimal("-345678901.23"),
                InterestCalculator.truncateToPic(new BigDecimal("-12345678901.23"), 9));
    }

    @Test
    void int12AddsCategoryInterestToTheRewrittenAccountTotal() {
        AccountRecord first = account("00000000001", "GROUP", "100.00", "3.00", "2.00");
        AccountRecord last = account("00000000002", "GROUP", "0.00", "0.00", "0.00");
        InterestRun run = calculator().run(input(
                List.of(category(first.acctId(), 1, "100.00"), category(first.acctId(), 2, "200.00"),
                        category(last.acctId(), 1, "0.00")),
                List.of(first, last),
                List.of(xref(first.acctId(), "0000000000000001"),
                        xref(last.acctId(), "0000000000000002")),
                List.of(disclosure("GROUP", 1, "12.00"), disclosure("GROUP", 2, "12.00"))));
        assertEquals(new BigDecimal("103.00"), run.accounts().get(0).currentBalance().value());
        assertEquals(new BigDecimal("0.00"), run.accounts().get(0).cycleCredit().value());
        assertEquals(3, run.transactions().size());
        assertEquals(new BigDecimal("0.00"), run.transactions().getLast().amount().value());
    }

    @Test
    void int14CounterSpansAccountsAndWrapsAfterSixDigits() {
        InterestRun run = calculator().run(input(
                List.of(category("00000000001", 1, "100.00"), category("00000000002", 1, "100.00")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00"),
                        account("00000000002", "GROUP", "0.00", "0.00", "0.00")),
                List.of(xref("00000000001", "0000000000000001"),
                        xref("00000000002", "0000000000000002")),
                List.of(disclosure("GROUP", 1, "12.00"))));
        assertEquals(PARM_DATE + "000001", run.transactions().get(0).id());
        assertEquals(PARM_DATE + "000002", run.transactions().get(1).id());
        assertEquals(0, InterestCalculator.nextTranIdSuffix(999_999));
    }

    @Test
    void int15PopulatesAllTransactionFieldsAndLeavesUnwrittenFieldsBlank() {
        InterestRun run = calculator().run(input(
                List.of(category("00000000001", 1, "100.00")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                List.of(xref("00000000001", "0000000000000001")),
                List.of(disclosure("GROUP", 1, "12.00"))));
        TransactionRecord transaction = run.transactions().getFirst();
        assertEquals(PARM_DATE + "000001", transaction.id());
        assertEquals("01", transaction.typeCode());
        assertEquals("0005", transaction.categoryCode());
        assertEquals("System    ", transaction.source());
        assertEquals("Int. for a/c 00000000001" + " ".repeat(76), transaction.description());
        assertEquals(new BigDecimal("1.00"), transaction.amount().value());
        assertEquals("000000000", transaction.merchantId());
        assertEquals(" ".repeat(50), transaction.merchantName());
        assertEquals(" ".repeat(50), transaction.merchantCity());
        assertEquals(" ".repeat(10), transaction.merchantZip());
        assertEquals("0000000000000001", transaction.cardNumber());
        assertEquals(" ".repeat(20), transaction.filler());
        assertNotEquals("", transaction.originalTimestamp());
        assertNotEquals("", transaction.processedTimestamp());
    }

    @Test
    void int16FormatsInjectedClockHundredthsForBothTimestamps() {
        InterestRun run = calculator().run(input(
                List.of(category("00000000001", 1, "100.00")),
                List.of(account("00000000001", "GROUP", "0.00", "0.00", "0.00")),
                List.of(xref("00000000001", "0000000000000001")),
                List.of(disclosure("GROUP", 1, "12.00"))));
        TransactionRecord transaction = run.transactions().getFirst();
        assertEquals("2022-07-18-12.34.56.120000", transaction.originalTimestamp());
        assertEquals(transaction.originalTimestamp(), transaction.processedTimestamp());
    }

    private static InterestCalculator calculator() {
        return new InterestCalculator(FIXED_CLOCK, PARM_DATE);
    }

    private static InterestInput input(
            List<TranCatBalanceRecord> categories,
            List<AccountRecord> accounts,
            List<CardXrefRecord> crossReferences,
            List<DisclosureGroupRecord> disclosures) {
        return new InterestInput(categories, accounts, crossReferences, disclosures);
    }

    private static AccountRecord account(
            String id, String group, String balance, String cycleCredit, String cycleDebit) {
        return new AccountRecord(
                id,
                "A",
                money(balance),
                money("500.00"),
                money("100.00"),
                "2020-01-01",
                "2030-01-01",
                "          ",
                money(cycleCredit),
                money(cycleDebit),
                "12345",
                padGroup(group),
                "");
    }

    private static TranCatBalanceRecord category(String accountId, int categoryCode, String balance) {
        return new TranCatBalanceRecord(
                accountId, "01", String.format("%04d", categoryCode), money(balance), "");
    }

    private static CardXrefRecord xref(String accountId, String cardNumber) {
        return new CardXrefRecord(cardNumber, "000000001", accountId, "");
    }

    private static DisclosureGroupRecord disclosure(String group, int categoryCode, String rate) {
        return new DisclosureGroupRecord(
                padGroup(group), "01", String.format("%04d", categoryCode), money(rate), "");
    }

    private static String padGroup(String group) {
        return group + " ".repeat(10 - group.length());
    }

    private static String display(TranCatBalanceRecord category) {
        return new String(TranCatBalanceRecord.CODEC.encode(category), StandardCharsets.US_ASCII);
    }

    private static ZonedDecimal money(String value) {
        return new ZonedDecimal(new BigDecimal(value));
    }
}
