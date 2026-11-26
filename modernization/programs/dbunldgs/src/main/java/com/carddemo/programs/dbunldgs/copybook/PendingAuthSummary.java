package com.carddemo.programs.dbunldgs.copybook;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Objects;

/**
 * Copybook CIPAUSMY: IMS root segment PAUTSUM0 (01 PENDING-AUTH-SUMMARY), 100 bytes.
 *
 * <pre>
 * off len field                     PIC
 *   0   6 PA-ACCT-ID                S9(11) COMP-3
 *   6   9 PA-CUST-ID                9(09)
 *  15   1 PA-AUTH-STATUS            X(01)
 *  16  10 PA-ACCOUNT-STATUS         X(02) OCCURS 5
 *  26   6 PA-CREDIT-LIMIT           S9(09)V99 COMP-3
 *  32   6 PA-CASH-LIMIT             S9(09)V99 COMP-3
 *  38   6 PA-CREDIT-BALANCE         S9(09)V99 COMP-3
 *  44   6 PA-CASH-BALANCE           S9(09)V99 COMP-3
 *  50   2 PA-APPROVED-AUTH-CNT      S9(04) COMP
 *  52   2 PA-DECLINED-AUTH-CNT      S9(04) COMP
 *  54   6 PA-APPROVED-AUTH-AMT      S9(09)V99 COMP-3
 *  60   6 PA-DECLINED-AUTH-AMT      S9(09)V99 COMP-3
 *  66  34 FILLER                    X(34)  (kept: GSAM output is byte-for-byte)
 * </pre>
 */
public record PendingAuthSummary(
        BigDecimal paAcctId,
        long paCustId,
        String paAuthStatus,
        List<String> paAccountStatus,
        BigDecimal paCreditLimit,
        BigDecimal paCashLimit,
        BigDecimal paCreditBalance,
        BigDecimal paCashBalance,
        short paApprovedAuthCnt,
        short paDeclinedAuthCnt,
        BigDecimal paApprovedAuthAmt,
        BigDecimal paDeclinedAuthAmt,
        String filler) {

    public static final int LENGTH = 100;
    public static final int ACCT_ID_OFFSET = 0;
    public static final int ACCT_ID_LENGTH = 6;
    public static final int ACCOUNT_STATUS_OCCURS = 5;

    public PendingAuthSummary {
        Objects.requireNonNull(paAccountStatus, "paAccountStatus");
        if (paAccountStatus.size() != ACCOUNT_STATUS_OCCURS) {
            throw new IllegalArgumentException("PA-ACCOUNT-STATUS OCCURS 5 TIMES");
        }
        paAuthStatus = CobolCodec.pic(paAuthStatus, 1);
        paAccountStatus = paAccountStatus.stream().map(s -> CobolCodec.pic(s, 2)).toList();
        filler = CobolCodec.pic(filler, 34);
        paAcctId = scaled(paAcctId, 0);
        paCreditLimit = scaled(paCreditLimit, 2);
        paCashLimit = scaled(paCashLimit, 2);
        paCreditBalance = scaled(paCreditBalance, 2);
        paCashBalance = scaled(paCashBalance, 2);
        paApprovedAuthAmt = scaled(paApprovedAuthAmt, 2);
        paDeclinedAuthAmt = scaled(paDeclinedAuthAmt, 2);
    }

    private static BigDecimal scaled(BigDecimal v, int scale) {
        return Objects.requireNonNull(v).setScale(scale, java.math.RoundingMode.DOWN);
    }

    /** PA-ACCT-ID IS NUMERIC, evaluated on the raw segment I/O area. */
    public static boolean isAcctIdNumeric(byte[] segment) {
        return CobolCodec.isNumericPacked(segment, ACCT_ID_OFFSET, ACCT_ID_LENGTH);
    }

    public static PendingAuthSummary fromBytes(byte[] b, Charset cs) {
        CobolCodec.requireLength(b, LENGTH, "PENDING-AUTH-SUMMARY");
        String[] status = new String[ACCOUNT_STATUS_OCCURS];
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            status[i] = CobolCodec.decodeAlpha(b, 16 + i * 2, 2, cs);
        }
        return new PendingAuthSummary(
                CobolCodec.decodePacked(b, 0, 11, 0),
                CobolCodec.decodeZoned(b, 6, 9, cs),
                CobolCodec.decodeAlpha(b, 15, 1, cs),
                List.of(status),
                CobolCodec.decodePacked(b, 26, 11, 2),
                CobolCodec.decodePacked(b, 32, 11, 2),
                CobolCodec.decodePacked(b, 38, 11, 2),
                CobolCodec.decodePacked(b, 44, 11, 2),
                CobolCodec.decodeHalfword(b, 50),
                CobolCodec.decodeHalfword(b, 52),
                CobolCodec.decodePacked(b, 54, 11, 2),
                CobolCodec.decodePacked(b, 60, 11, 2),
                CobolCodec.decodeAlpha(b, 66, 34, cs));
    }

    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        CobolCodec.encodePacked(paAcctId, b, 0, 11, 0);
        CobolCodec.encodeZoned(paCustId, b, 6, 9, cs);
        CobolCodec.encodeAlpha(paAuthStatus, b, 15, 1, cs);
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            CobolCodec.encodeAlpha(paAccountStatus.get(i), b, 16 + i * 2, 2, cs);
        }
        CobolCodec.encodePacked(paCreditLimit, b, 26, 11, 2);
        CobolCodec.encodePacked(paCashLimit, b, 32, 11, 2);
        CobolCodec.encodePacked(paCreditBalance, b, 38, 11, 2);
        CobolCodec.encodePacked(paCashBalance, b, 44, 11, 2);
        CobolCodec.encodeHalfword(paApprovedAuthCnt, b, 50);
        CobolCodec.encodeHalfword(paDeclinedAuthCnt, b, 52);
        CobolCodec.encodePacked(paApprovedAuthAmt, b, 54, 11, 2);
        CobolCodec.encodePacked(paDeclinedAuthAmt, b, 60, 11, 2);
        CobolCodec.encodeAlpha(filler, b, 66, 34, cs);
        return b;
    }
}
