package com.carddemo.programs.copaua0c.copybook;

import static com.carddemo.programs.copaua0c.support.Cobol.alpha;
import static com.carddemo.programs.copaua0c.support.Cobol.spaces;

import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;
import java.util.Arrays;

/**
 * CIPAUSMY - 01 PENDING-AUTH-SUMMARY, IMS root segment PAUTSUM0 (100 bytes incl. FILLER X(34)).
 * Amount fields are S9(09)V99 COMP-3; counters are S9(04) COMP.
 */
public final class PendingAuthSummary {

    public static final int SEGMENT_LENGTH = 100;
    public static final int AMOUNT_INT_DIGITS = 9;
    public static final int AMOUNT_SCALE = 2;
    public static final int COUNT_DIGITS = 4;

    private long acctId;                                              // PA-ACCT-ID        S9(11) COMP-3
    private long custId;                                              // PA-CUST-ID        9(09)
    private String authStatus = spaces(1);                            // PA-AUTH-STATUS    X(01)
    private final String[] accountStatus = {"  ", "  ", "  ", "  ", "  "}; // PA-ACCOUNT-STATUS X(02) OCCURS 5
    private BigDecimal creditLimit = zero();                          // PA-CREDIT-LIMIT   S9(09)V99 COMP-3
    private BigDecimal cashLimit = zero();                            // PA-CASH-LIMIT     S9(09)V99 COMP-3
    private BigDecimal creditBalance = zero();                        // PA-CREDIT-BALANCE S9(09)V99 COMP-3
    private BigDecimal cashBalance = zero();                          // PA-CASH-BALANCE   S9(09)V99 COMP-3
    private int approvedAuthCnt;                                      // PA-APPROVED-AUTH-CNT S9(04) COMP
    private int declinedAuthCnt;                                      // PA-DECLINED-AUTH-CNT S9(04) COMP
    private BigDecimal approvedAuthAmt = zero();                      // PA-APPROVED-AUTH-AMT S9(09)V99 COMP-3
    private BigDecimal declinedAuthAmt = zero();                      // PA-DECLINED-AUTH-AMT S9(09)V99 COMP-3

    private static BigDecimal zero() {
        return BigDecimal.ZERO.setScale(AMOUNT_SCALE);
    }

    private static BigDecimal amount(BigDecimal v) {
        return Cobol.truncate(v, AMOUNT_INT_DIGITS, AMOUNT_SCALE);
    }

    /** INITIALIZE PENDING-AUTH-SUMMARY REPLACING NUMERIC DATA BY ZERO (alphanumerics untouched). */
    public void initializeNumeric() {
        acctId = 0;
        custId = 0;
        creditLimit = zero();
        cashLimit = zero();
        creditBalance = zero();
        cashBalance = zero();
        approvedAuthCnt = 0;
        declinedAuthCnt = 0;
        approvedAuthAmt = zero();
        declinedAuthAmt = zero();
    }

    public long acctId() { return acctId; }
    public void acctId(long v) { acctId = v % 100_000_000_000L; }
    public long custId() { return custId; }
    public void custId(long v) { custId = Math.abs(v) % 1_000_000_000L; }
    public String authStatus() { return authStatus; }
    public void authStatus(String v) { authStatus = alpha(v, 1); }
    public String accountStatus(int index1) { return accountStatus[index1 - 1]; }
    public void accountStatus(int index1, String v) { accountStatus[index1 - 1] = alpha(v, 2); }
    public BigDecimal creditLimit() { return creditLimit; }
    public void creditLimit(BigDecimal v) { creditLimit = amount(v); }
    public BigDecimal cashLimit() { return cashLimit; }
    public void cashLimit(BigDecimal v) { cashLimit = amount(v); }
    public BigDecimal creditBalance() { return creditBalance; }
    public void creditBalance(BigDecimal v) { creditBalance = amount(v); }
    public BigDecimal cashBalance() { return cashBalance; }
    public void cashBalance(BigDecimal v) { cashBalance = amount(v); }
    public int approvedAuthCnt() { return approvedAuthCnt; }
    public void approvedAuthCnt(long v) { approvedAuthCnt = Cobol.truncateBinary(v, COUNT_DIGITS); }
    public int declinedAuthCnt() { return declinedAuthCnt; }
    public void declinedAuthCnt(long v) { declinedAuthCnt = Cobol.truncateBinary(v, COUNT_DIGITS); }
    public BigDecimal approvedAuthAmt() { return approvedAuthAmt; }
    public void approvedAuthAmt(BigDecimal v) { approvedAuthAmt = amount(v); }
    public BigDecimal declinedAuthAmt() { return declinedAuthAmt; }
    public void declinedAuthAmt(BigDecimal v) { declinedAuthAmt = amount(v); }

    public PendingAuthSummary copy() {
        PendingAuthSummary c = new PendingAuthSummary();
        c.acctId = acctId;
        c.custId = custId;
        c.authStatus = authStatus;
        System.arraycopy(accountStatus, 0, c.accountStatus, 0, accountStatus.length);
        c.creditLimit = creditLimit;
        c.cashLimit = cashLimit;
        c.creditBalance = creditBalance;
        c.cashBalance = cashBalance;
        c.approvedAuthCnt = approvedAuthCnt;
        c.declinedAuthCnt = declinedAuthCnt;
        c.approvedAuthAmt = approvedAuthAmt;
        c.declinedAuthAmt = declinedAuthAmt;
        return c;
    }

    @Override
    public String toString() {
        return "PendingAuthSummary[acctId=" + acctId + ", custId=" + custId + ", authStatus=" + authStatus
                + ", accountStatus=" + Arrays.toString(accountStatus) + ", creditLimit=" + creditLimit
                + ", cashLimit=" + cashLimit + ", creditBalance=" + creditBalance + ", cashBalance=" + cashBalance
                + ", approvedAuthCnt=" + approvedAuthCnt + ", declinedAuthCnt=" + declinedAuthCnt
                + ", approvedAuthAmt=" + approvedAuthAmt + ", declinedAuthAmt=" + declinedAuthAmt + "]";
    }
}
