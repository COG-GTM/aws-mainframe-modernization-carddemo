package com.carddemo.interest;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.carddemo.interest.model.AccountRecord;
import com.carddemo.interest.model.CardXrefRecord;
import com.carddemo.interest.model.TransactionRecord;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** Business rules of 1300-COMPUTE-INTEREST, 1300-B-WRITE-TX, 1050-UPDATE-ACCOUNT and 1400-COMPUTE-FEES. */
class InterestCalculatorTest {

    private final InterestCalculator calculator = new InterestCalculator();

    @ParameterizedTest(name = "balance {0} at {1}% -> {2}")
    @CsvSource({
            // balance,   rate,   expected monthly interest = trunc(balance * rate / 1200, 2)
            "1200.00,     12.00,  12.00",
            "1000.00,      0.01,   0.00",   // 0.008333 truncates to zero, not rounded up
            "1000.00,      1.00,   0.83",   // 0.8333 truncates, no ROUNDED phrase in the COMPUTE
            "0.00,        19.99,   0.00",   // zero balance yields no interest
            "-500.00,     12.00,  -5.00",   // credit balance yields negative (refunded) interest
            "-1000.00,     1.00,  -0.83",   // truncation is toward zero on negatives too
            "999999999.99, 0.01,  8333.33",
    })
    void computesMonthlyInterestWithCobolTruncation(String balance, String rate, String expected) {
        BigDecimal actual = calculator.computeMonthlyInterest(new BigDecimal(balance), new BigDecimal(rate));
        assertEquals(new BigDecimal(expected), actual);
    }

    @Test
    void monthlyInterestKeepsTwoDecimalScaleForZero() {
        BigDecimal interest = calculator.computeMonthlyInterest(new BigDecimal("0.00"), new BigDecimal("0.00"));
        assertEquals(0, interest.compareTo(BigDecimal.ZERO));
        assertEquals(2, interest.scale());
    }

    @Test
    void accumulatesTotalInterestWithinTheS9x9V99Field() {
        BigDecimal total = calculator.accumulate(new BigDecimal("10.55"), new BigDecimal("0.45"));
        assertEquals(new BigDecimal("11.00"), total);
    }

    @Test
    void accumulationWrapsWhenItOverflowsNineIntegerDigits() {
        // WS-TOTAL-INT is PIC S9(09)V99 and the COBOL ADD has no ON SIZE ERROR clause, so the
        // high-order digit is dropped rather than raising an error.
        BigDecimal total = calculator.accumulate(new BigDecimal("999999999.99"), new BigDecimal("0.02"));
        assertEquals(new BigDecimal("0.01"), total);
    }

    @Test
    void feeCalculationIsAnUnimplementedStubInTheLegacyProgram() {
        BigDecimal fees = calculator.computeFees(
                new com.carddemo.interest.model.TranCatBalRecord("1", "01", "5", new BigDecimal("100.00")),
                new BigDecimal("12.00"));
        assertEquals(0, fees.compareTo(BigDecimal.ZERO));
    }

    @Test
    void postsTotalInterestToBalanceAndClearsCycleTotals() {
        AccountRecord account = new AccountRecord(
                "00000000011", new BigDecimal("100.00"), new BigDecimal("25.00"), new BigDecimal("75.00"), "ZEROAPR");

        AccountRecord updated = calculator.applyInterestToAccount(account, new BigDecimal("1.25"));

        assertEquals(new BigDecimal("101.25"), updated.currentBalance());
        assertEquals(0, updated.currentCycleCredit().compareTo(BigDecimal.ZERO));
        assertEquals(0, updated.currentCycleDebit().compareTo(BigDecimal.ZERO));
        assertEquals("00000000011", updated.acctId());
        assertEquals("ZEROAPR   ", updated.groupId());
    }

    @Test
    void zeroTotalInterestStillClearsCycleTotals() {
        AccountRecord account = new AccountRecord(
                "11", new BigDecimal("0.00"), new BigDecimal("10.00"), new BigDecimal("10.00"), "DEFAULT");

        AccountRecord updated = calculator.applyInterestToAccount(account, CobolDecimal.ZERO_MONEY);

        assertEquals(0, updated.currentBalance().compareTo(BigDecimal.ZERO));
        assertEquals(0, updated.currentCycleCredit().compareTo(BigDecimal.ZERO));
        assertEquals(0, updated.currentCycleDebit().compareTo(BigDecimal.ZERO));
    }

    @Test
    void buildsInterestTransactionWithCopybookFieldWidths() {
        AccountRecord account = new AccountRecord(
                "12345678901", new BigDecimal("100.00"), BigDecimal.ZERO, BigDecimal.ZERO, "GROUP01");
        CardXrefRecord xref = new CardXrefRecord("4111111111111111", "123456789", "12345678901");

        TransactionRecord tx = calculator.buildInterestTransaction(
                "2022071800", 7, account, xref, new BigDecimal("12.34"),
                LocalDateTime.of(2022, 7, 18, 9, 30, 15, 120_000_000));

        assertEquals("2022071800000007", tx.tranId());
        assertEquals(16, tx.tranId().length());
        assertEquals("01", tx.typeCd());
        assertEquals("0005", tx.categoryCd());
        assertEquals("System    ", tx.source());
        assertEquals("Int. for a/c 12345678901", tx.description().strip());
        assertEquals(100, tx.description().length());
        assertEquals(new BigDecimal("12.34"), tx.amount());
        assertEquals(0L, tx.merchantId());
        assertEquals("4111111111111111", tx.cardNumber());
        assertEquals("2022-07-18-09.30.15.120000", tx.originTimestamp());
        assertEquals(tx.originTimestamp(), tx.processTimestamp());
        assertEquals(350, tx.toFixedLengthRecord().length());
    }

    @Test
    void fixedLengthRecordOverpunchesTheSignOnTheTrailingDigit() {
        AccountRecord account = new AccountRecord("11", BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, "G");
        CardXrefRecord xref = new CardXrefRecord("4111111111111111", "1", "11");

        TransactionRecord tx = calculator.buildInterestTransaction(
                "2022071800", 1, account, xref, new BigDecimal("-5.00"), LocalDateTime.of(2022, 7, 18, 0, 0));

        assertEquals("0000000050}", tx.toFixedLengthRecord().substring(132, 143));
    }
}
