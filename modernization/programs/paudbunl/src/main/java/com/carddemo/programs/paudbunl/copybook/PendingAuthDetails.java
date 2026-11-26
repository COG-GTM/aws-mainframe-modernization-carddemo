package com.carddemo.programs.paudbunl.copybook;

import com.carddemo.programs.paudbunl.support.CobolData;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Optional;

/**
 * CIPAUDTY - IMS segment PAUTDTL1, pending authorization details (child, 200 bytes).
 * Offsets follow the copybook exactly; FILLER X(17) occupies bytes 183-199.
 */
public record PendingAuthDetails(
        BigDecimal authDate9c,
        BigDecimal authTime9c,
        String authOrigDate,
        String authOrigTime,
        String cardNum,
        String authType,
        String cardExpiryDate,
        String messageType,
        String messageSource,
        String authIdCode,
        String authRespCode,
        String authRespReason,
        long processingCode,
        BigDecimal transactionAmt,
        BigDecimal approvedAmt,
        String merchantCategoryCode,
        String acqrCountryCode,
        int posEntryMode,
        String merchantId,
        String merchantName,
        String merchantCity,
        String merchantState,
        String merchantZip,
        String transactionId,
        String matchStatus,
        String authFraud,
        String fraudRptDate) {

    public static final int LENGTH = 200;
    /** PA-AUTHORIZATION-KEY (PA-AUTH-DATE-9C + PA-AUTH-TIME-9C), the PAUT9CTS sequence field. */
    public static final int AUTHORIZATION_KEY_LENGTH = 8;

    /** 88-levels on PA-MATCH-STATUS. */
    public enum MatchStatus {
        /** 88 PA-MATCH-PENDING VALUE 'P'. */
        PENDING("P"),
        /** 88 PA-MATCH-AUTH-DECLINED VALUE 'D'. */
        AUTH_DECLINED("D"),
        /** 88 PA-MATCH-PENDING-EXPIRED VALUE 'E'. */
        PENDING_EXPIRED("E"),
        /** 88 PA-MATCHED-WITH-TRAN VALUE 'M'. */
        MATCHED_WITH_TRAN("M");

        private final String code;

        MatchStatus(String code) {
            this.code = code;
        }

        public String code() {
            return code;
        }

        public static Optional<MatchStatus> fromCode(String code) {
            for (MatchStatus s : values()) {
                if (s.code.equals(code)) {
                    return Optional.of(s);
                }
            }
            return Optional.empty();
        }
    }

    /** 88-levels on PA-AUTH-FRAUD. */
    public enum AuthFraud {
        /** 88 PA-FRAUD-CONFIRMED VALUE 'F'. */
        CONFIRMED("F"),
        /** 88 PA-FRAUD-REMOVED VALUE 'R'. */
        REMOVED("R");

        private final String code;

        AuthFraud(String code) {
            this.code = code;
        }

        public String code() {
            return code;
        }

        public static Optional<AuthFraud> fromCode(String code) {
            for (AuthFraud f : values()) {
                if (f.code.equals(code)) {
                    return Optional.of(f);
                }
            }
            return Optional.empty();
        }
    }

    /** 88 PA-AUTH-APPROVED VALUE '00'. */
    public static final String AUTH_APPROVED_CODE = "00";

    public boolean isAuthApproved() {
        return AUTH_APPROVED_CODE.equals(authRespCode);
    }

    public boolean isMatchPending() {
        return MatchStatus.PENDING.code().equals(matchStatus);
    }

    public boolean isMatchAuthDeclined() {
        return MatchStatus.AUTH_DECLINED.code().equals(matchStatus);
    }

    public boolean isMatchPendingExpired() {
        return MatchStatus.PENDING_EXPIRED.code().equals(matchStatus);
    }

    public boolean isMatchedWithTran() {
        return MatchStatus.MATCHED_WITH_TRAN.code().equals(matchStatus);
    }

    public boolean isFraudConfirmed() {
        return AuthFraud.CONFIRMED.code().equals(authFraud);
    }

    public boolean isFraudRemoved() {
        return AuthFraud.REMOVED.code().equals(authFraud);
    }

    // Field layout: {offset, length}
    private static final int AUTH_DATE_9C = 0;     // S9(05) COMP-3, 3 bytes
    private static final int AUTH_TIME_9C = 3;     // S9(09) COMP-3, 5 bytes
    private static final int AUTH_ORIG_DATE = 8;   // X(06)
    private static final int AUTH_ORIG_TIME = 14;  // X(06)
    private static final int CARD_NUM = 20;        // X(16)
    private static final int AUTH_TYPE = 36;       // X(04)
    private static final int CARD_EXPIRY = 40;     // X(04)
    private static final int MESSAGE_TYPE = 44;    // X(06)
    private static final int MESSAGE_SOURCE = 50;  // X(06)
    private static final int AUTH_ID_CODE = 56;    // X(06)
    private static final int AUTH_RESP_CODE = 62;  // X(02)
    private static final int AUTH_RESP_REASON = 64; // X(04)
    private static final int PROCESSING_CODE = 68; // 9(06)
    private static final int TRANSACTION_AMT = 74; // S9(10)V99 COMP-3, 7 bytes
    private static final int APPROVED_AMT = 81;    // S9(10)V99 COMP-3, 7 bytes
    private static final int MERCHANT_CAT = 88;    // X(04)
    private static final int ACQR_COUNTRY = 92;    // X(03)
    private static final int POS_ENTRY_MODE = 95;  // 9(02)
    private static final int MERCHANT_ID = 97;     // X(15)
    private static final int MERCHANT_NAME = 112;  // X(22)
    private static final int MERCHANT_CITY = 134;  // X(13)
    private static final int MERCHANT_STATE = 147; // X(02)
    private static final int MERCHANT_ZIP = 149;   // X(09)
    private static final int TRANSACTION_ID = 158; // X(15)
    private static final int MATCH_STATUS = 173;   // X(01)
    private static final int AUTH_FRAUD = 174;     // X(01)
    private static final int FRAUD_RPT_DATE = 175; // X(08)
    public static final int FILLER_OFFSET = 183;   // X(17)

    public static PendingAuthDetails fromBytes(byte[] b, Charset cs) {
        if (b.length != LENGTH) {
            throw new IllegalArgumentException("PAUTDTL1 segment must be " + LENGTH + " bytes, got " + b.length);
        }
        return new PendingAuthDetails(
                CobolData.decodePacked(b, AUTH_DATE_9C, 3, 0),
                CobolData.decodePacked(b, AUTH_TIME_9C, 5, 0),
                a(b, AUTH_ORIG_DATE, 6, cs),
                a(b, AUTH_ORIG_TIME, 6, cs),
                a(b, CARD_NUM, 16, cs),
                a(b, AUTH_TYPE, 4, cs),
                a(b, CARD_EXPIRY, 4, cs),
                a(b, MESSAGE_TYPE, 6, cs),
                a(b, MESSAGE_SOURCE, 6, cs),
                a(b, AUTH_ID_CODE, 6, cs),
                a(b, AUTH_RESP_CODE, 2, cs),
                a(b, AUTH_RESP_REASON, 4, cs),
                CobolData.decodeZoned(b, PROCESSING_CODE, 6, cs),
                CobolData.decodePacked(b, TRANSACTION_AMT, 7, 2),
                CobolData.decodePacked(b, APPROVED_AMT, 7, 2),
                a(b, MERCHANT_CAT, 4, cs),
                a(b, ACQR_COUNTRY, 3, cs),
                (int) CobolData.decodeZoned(b, POS_ENTRY_MODE, 2, cs),
                a(b, MERCHANT_ID, 15, cs),
                a(b, MERCHANT_NAME, 22, cs),
                a(b, MERCHANT_CITY, 13, cs),
                a(b, MERCHANT_STATE, 2, cs),
                a(b, MERCHANT_ZIP, 9, cs),
                a(b, TRANSACTION_ID, 15, cs),
                a(b, MATCH_STATUS, 1, cs),
                a(b, AUTH_FRAUD, 1, cs),
                a(b, FRAUD_RPT_DATE, 8, cs));
    }

    /** Serialises the segment; FILLER is written as spaces. */
    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        CobolData.encodeAlpha("", b, 0, LENGTH, cs);
        CobolData.encodePacked(authDate9c, 5, 0, b, AUTH_DATE_9C);
        CobolData.encodePacked(authTime9c, 9, 0, b, AUTH_TIME_9C);
        CobolData.encodeAlpha(authOrigDate, b, AUTH_ORIG_DATE, 6, cs);
        CobolData.encodeAlpha(authOrigTime, b, AUTH_ORIG_TIME, 6, cs);
        CobolData.encodeAlpha(cardNum, b, CARD_NUM, 16, cs);
        CobolData.encodeAlpha(authType, b, AUTH_TYPE, 4, cs);
        CobolData.encodeAlpha(cardExpiryDate, b, CARD_EXPIRY, 4, cs);
        CobolData.encodeAlpha(messageType, b, MESSAGE_TYPE, 6, cs);
        CobolData.encodeAlpha(messageSource, b, MESSAGE_SOURCE, 6, cs);
        CobolData.encodeAlpha(authIdCode, b, AUTH_ID_CODE, 6, cs);
        CobolData.encodeAlpha(authRespCode, b, AUTH_RESP_CODE, 2, cs);
        CobolData.encodeAlpha(authRespReason, b, AUTH_RESP_REASON, 4, cs);
        CobolData.encodeZoned(processingCode, b, PROCESSING_CODE, 6, cs);
        CobolData.encodePacked(transactionAmt, 12, 2, b, TRANSACTION_AMT);
        CobolData.encodePacked(approvedAmt, 12, 2, b, APPROVED_AMT);
        CobolData.encodeAlpha(merchantCategoryCode, b, MERCHANT_CAT, 4, cs);
        CobolData.encodeAlpha(acqrCountryCode, b, ACQR_COUNTRY, 3, cs);
        CobolData.encodeZoned(posEntryMode, b, POS_ENTRY_MODE, 2, cs);
        CobolData.encodeAlpha(merchantId, b, MERCHANT_ID, 15, cs);
        CobolData.encodeAlpha(merchantName, b, MERCHANT_NAME, 22, cs);
        CobolData.encodeAlpha(merchantCity, b, MERCHANT_CITY, 13, cs);
        CobolData.encodeAlpha(merchantState, b, MERCHANT_STATE, 2, cs);
        CobolData.encodeAlpha(merchantZip, b, MERCHANT_ZIP, 9, cs);
        CobolData.encodeAlpha(transactionId, b, TRANSACTION_ID, 15, cs);
        CobolData.encodeAlpha(matchStatus, b, MATCH_STATUS, 1, cs);
        CobolData.encodeAlpha(authFraud, b, AUTH_FRAUD, 1, cs);
        CobolData.encodeAlpha(fraudRptDate, b, FRAUD_RPT_DATE, 8, cs);
        return b;
    }

    private static String a(byte[] b, int off, int len, Charset cs) {
        return CobolData.decodeAlpha(b, off, len, cs);
    }
}
