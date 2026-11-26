package com.carddemo.programs.cbpaup0c.copybook;

import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodePacked;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodePackedLong;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.decodeText;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.encodePacked;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.encodeText;
import static com.carddemo.programs.cbpaup0c.copybook.CobolCodec.fit;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Optional;

/**
 * CIPAUDTY - IMS segment PAUTDTL1, pending authorization details (child of PAUTSUM0, 200 bytes).
 * PIC 9 DISPLAY fields are kept as their exact zoned text so a segment round-trips unchanged.
 */
public record PendingAuthDetails(
        AuthorizationKey authorizationKey, // PA-AUTHORIZATION-KEY                       @0   (8)
        String authOrigDate,               // PA-AUTH-ORIG-DATE         X(06)            @8   (6)
        String authOrigTime,               // PA-AUTH-ORIG-TIME         X(06)            @14  (6)
        String cardNum,                    // PA-CARD-NUM               X(16)            @20  (16)
        String authType,                   // PA-AUTH-TYPE              X(04)            @36  (4)
        String cardExpiryDate,             // PA-CARD-EXPIRY-DATE       X(04)            @40  (4)
        String messageType,                // PA-MESSAGE-TYPE           X(06)            @44  (6)
        String messageSource,              // PA-MESSAGE-SOURCE         X(06)            @50  (6)
        String authIdCode,                 // PA-AUTH-ID-CODE           X(06)            @56  (6)
        String authRespCode,               // PA-AUTH-RESP-CODE         X(02)            @62  (2)
        String authRespReason,             // PA-AUTH-RESP-REASON       X(04)            @64  (4)
        String processingCode,             // PA-PROCESSING-CODE        9(06)            @68  (6)
        BigDecimal transactionAmt,         // PA-TRANSACTION-AMT        S9(10)V99 COMP-3 @74  (7)
        BigDecimal approvedAmt,            // PA-APPROVED-AMT           S9(10)V99 COMP-3 @81  (7)
        String merchantCategoryCode,       // PA-MERCHANT-CATAGORY-CODE X(04)            @88  (4)
        String acqrCountryCode,            // PA-ACQR-COUNTRY-CODE      X(03)            @92  (3)
        String posEntryMode,               // PA-POS-ENTRY-MODE         9(02)            @95  (2)
        String merchantId,                 // PA-MERCHANT-ID            X(15)            @97  (15)
        String merchantName,               // PA-MERCHANT-NAME          X(22)            @112 (22)
        String merchantCity,               // PA-MERCHANT-CITY          X(13)            @134 (13)
        String merchantState,              // PA-MERCHANT-STATE         X(02)            @147 (2)
        String merchantZip,                // PA-MERCHANT-ZIP           X(09)            @149 (9)
        String transactionId,              // PA-TRANSACTION-ID         X(15)            @158 (15)
        String matchStatus,                // PA-MATCH-STATUS           X(01)            @173 (1)
        String authFraud,                  // PA-AUTH-FRAUD             X(01)            @174 (1)
        String fraudRptDate,               // PA-FRAUD-RPT-DATE         X(08)            @175 (8)
        String filler) {                   // FILLER                    X(17)            @183 (17)

    public static final String SEGMENT_NAME = "PAUTDTL1";
    public static final int LENGTH = 200;
    public static final String AUTH_APPROVED = "00";

    /** PA-AUTHORIZATION-KEY: PA-AUTH-DATE-9C S9(05) COMP-3 + PA-AUTH-TIME-9C S9(09) COMP-3 (both 9s-complemented). */
    public record AuthorizationKey(int authDate9c, long authTime9c) {
    }

    /** 88-levels on PA-MATCH-STATUS. */
    public enum MatchStatus {
        PENDING("P"), AUTH_DECLINED("D"), PENDING_EXPIRED("E"), MATCHED_WITH_TRAN("M");

        private final String code;

        MatchStatus(String code) {
            this.code = code;
        }

        public String code() {
            return code;
        }

        public static Optional<MatchStatus> of(String code) {
            for (MatchStatus m : values()) {
                if (m.code.equals(code)) {
                    return Optional.of(m);
                }
            }
            return Optional.empty();
        }
    }

    /** 88-levels on PA-AUTH-FRAUD. */
    public enum AuthFraud {
        FRAUD_CONFIRMED("F"), FRAUD_REMOVED("R");

        private final String code;

        AuthFraud(String code) {
            this.code = code;
        }

        public String code() {
            return code;
        }

        public static Optional<AuthFraud> of(String code) {
            for (AuthFraud f : values()) {
                if (f.code.equals(code)) {
                    return Optional.of(f);
                }
            }
            return Optional.empty();
        }
    }

    public int authDate9c() {
        return authorizationKey.authDate9c();
    }

    public long authTime9c() {
        return authorizationKey.authTime9c();
    }

    /** 88 PA-AUTH-APPROVED VALUE '00'. */
    public boolean isAuthApproved() {
        return AUTH_APPROVED.equals(authRespCode);
    }

    /** 88 PA-MATCH-PENDING VALUE 'P'. */
    public boolean isMatchPending() {
        return MatchStatus.PENDING.code().equals(matchStatus);
    }

    /** 88 PA-MATCH-AUTH-DECLINED VALUE 'D'. */
    public boolean isMatchAuthDeclined() {
        return MatchStatus.AUTH_DECLINED.code().equals(matchStatus);
    }

    /** 88 PA-MATCH-PENDING-EXPIRED VALUE 'E'. */
    public boolean isMatchPendingExpired() {
        return MatchStatus.PENDING_EXPIRED.code().equals(matchStatus);
    }

    /** 88 PA-MATCHED-WITH-TRAN VALUE 'M'. */
    public boolean isMatchedWithTran() {
        return MatchStatus.MATCHED_WITH_TRAN.code().equals(matchStatus);
    }

    /** 88 PA-FRAUD-CONFIRMED VALUE 'F'. */
    public boolean isFraudConfirmed() {
        return AuthFraud.FRAUD_CONFIRMED.code().equals(authFraud);
    }

    /** 88 PA-FRAUD-REMOVED VALUE 'R'. */
    public boolean isFraudRemoved() {
        return AuthFraud.FRAUD_REMOVED.code().equals(authFraud);
    }

    public static PendingAuthDetails fromBytes(byte[] b, Charset cs) {
        if (b.length < LENGTH) {
            throw new IllegalArgumentException("PAUTDTL1 segment must be " + LENGTH + " bytes, got " + b.length);
        }
        return new PendingAuthDetails(
                new AuthorizationKey((int) decodePackedLong(b, 0, 3), decodePackedLong(b, 3, 5)),
                decodeText(b, 8, 6, cs), decodeText(b, 14, 6, cs), decodeText(b, 20, 16, cs),
                decodeText(b, 36, 4, cs), decodeText(b, 40, 4, cs), decodeText(b, 44, 6, cs),
                decodeText(b, 50, 6, cs), decodeText(b, 56, 6, cs), decodeText(b, 62, 2, cs),
                decodeText(b, 64, 4, cs), decodeText(b, 68, 6, cs),
                decodePacked(b, 74, 7, 2), decodePacked(b, 81, 7, 2),
                decodeText(b, 88, 4, cs), decodeText(b, 92, 3, cs), decodeText(b, 95, 2, cs),
                decodeText(b, 97, 15, cs), decodeText(b, 112, 22, cs), decodeText(b, 134, 13, cs),
                decodeText(b, 147, 2, cs), decodeText(b, 149, 9, cs), decodeText(b, 158, 15, cs),
                decodeText(b, 173, 1, cs), decodeText(b, 174, 1, cs), decodeText(b, 175, 8, cs),
                decodeText(b, 183, 17, cs));
    }

    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        encodePacked(authorizationKey.authDate9c(), b, 0, 3);
        encodePacked(authorizationKey.authTime9c(), b, 3, 5);
        encodeText(authOrigDate, b, 8, 6, cs);
        encodeText(authOrigTime, b, 14, 6, cs);
        encodeText(cardNum, b, 20, 16, cs);
        encodeText(authType, b, 36, 4, cs);
        encodeText(cardExpiryDate, b, 40, 4, cs);
        encodeText(messageType, b, 44, 6, cs);
        encodeText(messageSource, b, 50, 6, cs);
        encodeText(authIdCode, b, 56, 6, cs);
        encodeText(authRespCode, b, 62, 2, cs);
        encodeText(authRespReason, b, 64, 4, cs);
        encodeText(processingCode, b, 68, 6, cs);
        encodePacked(transactionAmt, 2, b, 74, 7);
        encodePacked(approvedAmt, 2, b, 81, 7);
        encodeText(merchantCategoryCode, b, 88, 4, cs);
        encodeText(acqrCountryCode, b, 92, 3, cs);
        encodeText(posEntryMode, b, 95, 2, cs);
        encodeText(merchantId, b, 97, 15, cs);
        encodeText(merchantName, b, 112, 22, cs);
        encodeText(merchantCity, b, 134, 13, cs);
        encodeText(merchantState, b, 147, 2, cs);
        encodeText(merchantZip, b, 149, 9, cs);
        encodeText(transactionId, b, 158, 15, cs);
        encodeText(matchStatus, b, 173, 1, cs);
        encodeText(authFraud, b, 174, 1, cs);
        encodeText(fraudRptDate, b, 175, 8, cs);
        encodeText(filler, b, 183, 17, cs);
        return b;
    }

    /** Convenience builder for tests and callers that only care about the fields CBPAUP0C reads. */
    public static PendingAuthDetails of(int authDate9c, long authTime9c, String respCode,
                                        BigDecimal transactionAmt, BigDecimal approvedAmt) {
        return new PendingAuthDetails(new AuthorizationKey(authDate9c, authTime9c),
                fit("", 6), fit("", 6), fit("", 16), fit("", 4), fit("", 4), fit("", 6), fit("", 6),
                fit("", 6), fit(respCode, 2), fit("", 4), "000000", transactionAmt.setScale(2),
                approvedAmt.setScale(2), fit("", 4), fit("", 3), "00", fit("", 15), fit("", 22),
                fit("", 13), fit("", 2), fit("", 9), fit("", 15), "P", " ", fit("", 8), fit("", 17));
    }
}
