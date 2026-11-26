package com.carddemo.programs.paudblod.copybook;

import static com.carddemo.programs.paudblod.copybook.CobolFields.*;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.List;

/**
 * Copybook CIPAUSMY: IMS segment PAUTSUM0 (pending authorization summary, root), 100 bytes.
 * {@code 01 PENDING-AUTH-SUMMARY} in PAUDBLOD.
 */
public record PendingAuthSummary(
        /* 05 PA-ACCT-ID          PIC S9(11) COMP-3       */ BigDecimal paAcctId,
        /* 05 PA-CUST-ID          PIC  9(09)              */ long paCustId,
        /* 05 PA-AUTH-STATUS      PIC  X(01)              */ String paAuthStatus,
        /* 05 PA-ACCOUNT-STATUS   PIC  X(02) OCCURS 5     */ List<String> paAccountStatus,
        /* 05 PA-CREDIT-LIMIT     PIC S9(09)V99 COMP-3    */ BigDecimal paCreditLimit,
        /* 05 PA-CASH-LIMIT       PIC S9(09)V99 COMP-3    */ BigDecimal paCashLimit,
        /* 05 PA-CREDIT-BALANCE   PIC S9(09)V99 COMP-3    */ BigDecimal paCreditBalance,
        /* 05 PA-CASH-BALANCE     PIC S9(09)V99 COMP-3    */ BigDecimal paCashBalance,
        /* 05 PA-APPROVED-AUTH-CNT PIC S9(04) COMP        */ int paApprovedAuthCnt,
        /* 05 PA-DECLINED-AUTH-CNT PIC S9(04) COMP        */ int paDeclinedAuthCnt,
        /* 05 PA-APPROVED-AUTH-AMT PIC S9(09)V99 COMP-3   */ BigDecimal paApprovedAuthAmt,
        /* 05 PA-DECLINED-AUTH-AMT PIC S9(09)V99 COMP-3   */ BigDecimal paDeclinedAuthAmt,
        /* 05 FILLER              PIC  X(34)              */ String filler) {

    public static final int LENGTH = 100;
    public static final int ACCOUNT_STATUS_OCCURS = 5;

    public static final int OFF_ACCT_ID = 0;
    public static final int LEN_ACCT_ID = 6;
    public static final int OFF_CUST_ID = 6;
    public static final int OFF_AUTH_STATUS = 15;
    public static final int OFF_ACCOUNT_STATUS = 16;
    public static final int OFF_CREDIT_LIMIT = 26;
    public static final int OFF_CASH_LIMIT = 32;
    public static final int OFF_CREDIT_BALANCE = 38;
    public static final int OFF_CASH_BALANCE = 44;
    public static final int OFF_APPROVED_AUTH_CNT = 50;
    public static final int OFF_DECLINED_AUTH_CNT = 52;
    public static final int OFF_APPROVED_AUTH_AMT = 54;
    public static final int OFF_DECLINED_AUTH_AMT = 60;
    public static final int OFF_FILLER = 66;

    public PendingAuthSummary {
        paAuthStatus = fixed(paAuthStatus, 1);
        if (paAccountStatus == null || paAccountStatus.size() != ACCOUNT_STATUS_OCCURS) {
            throw new IllegalArgumentException("PA-ACCOUNT-STATUS OCCURS 5 TIMES");
        }
        paAccountStatus = paAccountStatus.stream().map(s -> fixed(s, 2)).toList();
        filler = fixed(filler, 34);
    }

    public static PendingAuthSummary fromBytes(byte[] b, Charset cs) {
        if (b.length != LENGTH) {
            throw new IllegalArgumentException("PAUTSUM0 must be " + LENGTH + " bytes, got " + b.length);
        }
        String[] status = new String[ACCOUNT_STATUS_OCCURS];
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            status[i] = decodeText(b, OFF_ACCOUNT_STATUS + i * 2, 2, cs);
        }
        return new PendingAuthSummary(
                decodePacked(b, OFF_ACCT_ID, 6, 0),
                decodeZoned(b, OFF_CUST_ID, 9, cs),
                decodeText(b, OFF_AUTH_STATUS, 1, cs),
                List.of(status),
                decodePacked(b, OFF_CREDIT_LIMIT, 6, 2),
                decodePacked(b, OFF_CASH_LIMIT, 6, 2),
                decodePacked(b, OFF_CREDIT_BALANCE, 6, 2),
                decodePacked(b, OFF_CASH_BALANCE, 6, 2),
                decodeBinary(b, OFF_APPROVED_AUTH_CNT, 2),
                decodeBinary(b, OFF_DECLINED_AUTH_CNT, 2),
                decodePacked(b, OFF_APPROVED_AUTH_AMT, 6, 2),
                decodePacked(b, OFF_DECLINED_AUTH_AMT, 6, 2),
                decodeText(b, OFF_FILLER, 34, cs));
    }

    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        encodePacked(paAcctId, 11, 0, b, OFF_ACCT_ID);
        encodeZoned(paCustId, 9, b, OFF_CUST_ID, cs);
        encodeText(paAuthStatus, 1, b, OFF_AUTH_STATUS, cs);
        for (int i = 0; i < ACCOUNT_STATUS_OCCURS; i++) {
            encodeText(paAccountStatus.get(i), 2, b, OFF_ACCOUNT_STATUS + i * 2, cs);
        }
        encodePacked(paCreditLimit, 11, 2, b, OFF_CREDIT_LIMIT);
        encodePacked(paCashLimit, 11, 2, b, OFF_CASH_LIMIT);
        encodePacked(paCreditBalance, 11, 2, b, OFF_CREDIT_BALANCE);
        encodePacked(paCashBalance, 11, 2, b, OFF_CASH_BALANCE);
        encodeBinary(paApprovedAuthCnt, 4, 2, b, OFF_APPROVED_AUTH_CNT);
        encodeBinary(paDeclinedAuthCnt, 4, 2, b, OFF_DECLINED_AUTH_CNT);
        encodePacked(paApprovedAuthAmt, 11, 2, b, OFF_APPROVED_AUTH_AMT);
        encodePacked(paDeclinedAuthAmt, 11, 2, b, OFF_DECLINED_AUTH_AMT);
        encodeText(filler, 34, b, OFF_FILLER, cs);
        return b;
    }
}
