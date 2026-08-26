package com.carddemo.interest;

import com.carddemo.interest.model.AccountRecord;
import com.carddemo.interest.model.CardXrefRecord;
import com.carddemo.interest.model.TranCatBalRecord;
import com.carddemo.interest.model.TransactionRecord;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * The arithmetic of CBACT04C, free of file I/O: monthly interest per transaction category, the
 * interest transaction that records it, and the account balance update that posts it.
 */
public final class InterestCalculator {

    /** Divisor of 1300-COMPUTE-INTEREST: percent-to-fraction (100) times months per year (12). */
    private static final BigDecimal MONTHLY_DIVISOR = BigDecimal.valueOf(1200);

    private static final String INTEREST_TRAN_TYPE_CD = "01";
    private static final String INTEREST_TRAN_CAT_CD = "05";
    private static final String INTEREST_TRAN_SOURCE = "System";
    private static final String INTEREST_DESC_PREFIX = "Int. for a/c ";

    /**
     * 1300-COMPUTE-INTEREST: {@code COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200}.
     *
     * <p>No ROUNDED phrase is coded, so the result is truncated toward zero into the
     * {@code S9(09)V99} receiving field.
     */
    public BigDecimal computeMonthlyInterest(BigDecimal categoryBalance, BigDecimal annualRatePercent) {
        BigDecimal raw = categoryBalance.multiply(annualRatePercent)
                .divide(MONTHLY_DIVISOR, CobolDecimal.MONEY_SCALE + 6, java.math.RoundingMode.DOWN);
        return CobolDecimal.toAmount(raw);
    }

    /** {@code ADD WS-MONTHLY-INT TO WS-TOTAL-INT} — accumulation stays inside {@code S9(09)V99}. */
    public BigDecimal accumulate(BigDecimal totalInterest, BigDecimal monthlyInterest) {
        return CobolDecimal.toAmount(totalInterest.add(monthlyInterest));
    }

    /**
     * 1400-COMPUTE-FEES: the COBOL paragraph is an empty stub ("To be implemented"), so the port
     * keeps it as an explicit no-op rather than inventing behaviour.
     */
    public BigDecimal computeFees(TranCatBalRecord categoryBalance, BigDecimal interestRate) {
        return CobolDecimal.ZERO_MONEY;
    }

    /**
     * 1050-UPDATE-ACCOUNT: {@code ADD WS-TOTAL-INT TO ACCT-CURR-BAL}, then zero the cycle credit and
     * debit totals.
     */
    public AccountRecord applyInterestToAccount(AccountRecord account, BigDecimal totalInterest) {
        return new AccountRecord(
                account.acctId(),
                CobolDecimal.toBalance(account.currentBalance().add(totalInterest)),
                CobolDecimal.ZERO_MONEY,
                CobolDecimal.ZERO_MONEY,
                account.groupId());
    }

    /**
     * 1300-B-WRITE-TX: builds the interest transaction. {@code TRAN-ID} is the 10-character run date
     * from the PARM concatenated with a 6-digit sequence number, giving the {@code X(16)} key.
     */
    public TransactionRecord buildInterestTransaction(
            String runDate,
            long sequenceNumber,
            AccountRecord account,
            CardXrefRecord xref,
            BigDecimal monthlyInterest,
            LocalDateTime timestamp) {
        String tranId = CobolDecimal.alphanumeric(runDate, 10) + CobolDecimal.zoned(sequenceNumber, 6);
        String db2Timestamp = Db2Timestamp.format(timestamp);
        return new TransactionRecord(
                tranId,
                INTEREST_TRAN_TYPE_CD,
                INTEREST_TRAN_CAT_CD,
                INTEREST_TRAN_SOURCE,
                INTEREST_DESC_PREFIX + account.acctId(),
                monthlyInterest,
                0L,
                "",
                "",
                "",
                xref.cardNumber(),
                db2Timestamp,
                db2Timestamp);
    }
}
