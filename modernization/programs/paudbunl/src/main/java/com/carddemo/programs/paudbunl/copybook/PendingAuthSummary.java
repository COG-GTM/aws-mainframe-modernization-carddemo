package com.carddemo.programs.paudbunl.copybook;

import com.carddemo.programs.paudbunl.support.CobolData;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Objects;

/**
 * CIPAUSMY - IMS segment PAUTSUM0, pending authorization summary (root, 100 bytes).
 *
 * <pre>
 * 05 PA-ACCT-ID            PIC S9(11)    COMP-3   offset  0 len 6
 * 05 PA-CUST-ID            PIC  9(09)             offset  6 len 9
 * 05 PA-AUTH-STATUS        PIC  X(01)             offset 15 len 1
 * 05 PA-ACCOUNT-STATUS     PIC  X(02) OCCURS 5    offset 16 len 10
 * 05 PA-CREDIT-LIMIT       PIC S9(09)V99 COMP-3   offset 26 len 6
 * 05 PA-CASH-LIMIT         PIC S9(09)V99 COMP-3   offset 32 len 6
 * 05 PA-CREDIT-BALANCE     PIC S9(09)V99 COMP-3   offset 38 len 6
 * 05 PA-CASH-BALANCE       PIC S9(09)V99 COMP-3   offset 44 len 6
 * 05 PA-APPROVED-AUTH-CNT  PIC S9(04)    COMP     offset 50 len 2
 * 05 PA-DECLINED-AUTH-CNT  PIC S9(04)    COMP     offset 52 len 2
 * 05 PA-APPROVED-AUTH-AMT  PIC S9(09)V99 COMP-3   offset 54 len 6
 * 05 PA-DECLINED-AUTH-AMT  PIC S9(09)V99 COMP-3   offset 60 len 6
 * 05 FILLER                PIC  X(34)             offset 66 len 34
 * </pre>
 */
public record PendingAuthSummary(
        BigDecimal acctId,
        long custId,
        String authStatus,
        List<String> accountStatus,
        BigDecimal creditLimit,
        BigDecimal cashLimit,
        BigDecimal creditBalance,
        BigDecimal cashBalance,
        int approvedAuthCnt,
        int declinedAuthCnt,
        BigDecimal approvedAuthAmt,
        BigDecimal declinedAuthAmt) {

    public static final int LENGTH = 100;
    public static final int ACCOUNT_STATUS_OCCURS = 5;

    public static final int ACCT_ID_OFFSET = 0;
    public static final int ACCT_ID_DIGITS = 11;
    public static final int ACCT_ID_LENGTH = CobolData.packedLength(ACCT_ID_DIGITS);
    private static final int CUST_ID = 6;
    private static final int AUTH_STATUS = 15;
    private static final int ACCOUNT_STATUS = 16;
    private static final int CREDIT_LIMIT = 26;
    private static final int CASH_LIMIT = 32;
    private static final int CREDIT_BALANCE = 38;
    private static final int CASH_BALANCE = 44;
    private static final int APPROVED_AUTH_CNT = 50;
    private static final int DECLINED_AUTH_CNT = 52;
    private static final int APPROVED_AUTH_AMT = 54;
    private static final int DECLINED_AUTH_AMT = 60;
    private static final int AMT_DIGITS = 11;
    private static final int AMT_SCALE = 2;
    private static final int AMT_LEN = CobolData.packedLength(AMT_DIGITS);

    public PendingAuthSummary {
        Objects.requireNonNull(accountStatus, "accountStatus");
        if (accountStatus.size() != ACCOUNT_STATUS_OCCURS) {
            throw new IllegalArgumentException("PA-ACCOUNT-STATUS OCCURS 5 TIMES, got " + accountStatus.size());
        }
        accountStatus = List.copyOf(accountStatus);
    }

    /** {@code PA-ACCT-ID IS NUMERIC} evaluated on the raw segment bytes. */
    public static boolean isAcctIdNumeric(byte[] segment) {
        return CobolData.isNumericPacked(segment, ACCT_ID_OFFSET, ACCT_ID_LENGTH);
    }

    public static PendingAuthSummary fromBytes(byte[] b, Charset cs) {
        if (b.length != LENGTH) {
            throw new IllegalArgumentException("PAUTSUM0 segment must be " + LENGTH + " bytes, got " + b.length);
        }
        String[] status = new String[ACCOUNT_STATUS_OCCURS];
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            status[i] = CobolData.decodeAlpha(b, ACCOUNT_STATUS + i * 2, 2, cs);
        }
        return new PendingAuthSummary(
                CobolData.decodePacked(b, ACCT_ID_OFFSET, ACCT_ID_LENGTH, 0),
                CobolData.decodeZoned(b, CUST_ID, 9, cs),
                CobolData.decodeAlpha(b, AUTH_STATUS, 1, cs),
                List.of(status),
                CobolData.decodePacked(b, CREDIT_LIMIT, AMT_LEN, AMT_SCALE),
                CobolData.decodePacked(b, CASH_LIMIT, AMT_LEN, AMT_SCALE),
                CobolData.decodePacked(b, CREDIT_BALANCE, AMT_LEN, AMT_SCALE),
                CobolData.decodePacked(b, CASH_BALANCE, AMT_LEN, AMT_SCALE),
                (int) CobolData.decodeBinary(b, APPROVED_AUTH_CNT, 2),
                (int) CobolData.decodeBinary(b, DECLINED_AUTH_CNT, 2),
                CobolData.decodePacked(b, APPROVED_AUTH_AMT, AMT_LEN, AMT_SCALE),
                CobolData.decodePacked(b, DECLINED_AUTH_AMT, AMT_LEN, AMT_SCALE));
    }

    /** Serialises the segment; FILLER is written as spaces. */
    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        CobolData.encodeAlpha("", b, 0, LENGTH, cs);
        CobolData.encodePacked(acctId, ACCT_ID_DIGITS, 0, b, ACCT_ID_OFFSET);
        CobolData.encodeZoned(custId, b, CUST_ID, 9, cs);
        CobolData.encodeAlpha(authStatus, b, AUTH_STATUS, 1, cs);
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            CobolData.encodeAlpha(accountStatus.get(i), b, ACCOUNT_STATUS + i * 2, 2, cs);
        }
        CobolData.encodePacked(creditLimit, AMT_DIGITS, AMT_SCALE, b, CREDIT_LIMIT);
        CobolData.encodePacked(cashLimit, AMT_DIGITS, AMT_SCALE, b, CASH_LIMIT);
        CobolData.encodePacked(creditBalance, AMT_DIGITS, AMT_SCALE, b, CREDIT_BALANCE);
        CobolData.encodePacked(cashBalance, AMT_DIGITS, AMT_SCALE, b, CASH_BALANCE);
        CobolData.encodeBinary(approvedAuthCnt, b, APPROVED_AUTH_CNT, 2);
        CobolData.encodeBinary(declinedAuthCnt, b, DECLINED_AUTH_CNT, 2);
        CobolData.encodePacked(approvedAuthAmt, AMT_DIGITS, AMT_SCALE, b, APPROVED_AUTH_AMT);
        CobolData.encodePacked(declinedAuthAmt, AMT_DIGITS, AMT_SCALE, b, DECLINED_AUTH_AMT);
        return b;
    }
}
