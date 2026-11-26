package com.carddemo.programs.dbunldgs.copybook;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Optional;

/**
 * Copybook CIPAUDTY: IMS child segment PAUTDTL1 (01 PENDING-AUTH-DETAILS), 200 bytes.
 *
 * <pre>
 * off len field                         PIC
 *   0   3 PA-AUTH-DATE-9C               S9(05) COMP-3   } PA-AUTHORIZATION-KEY
 *   3   5 PA-AUTH-TIME-9C               S9(09) COMP-3   }
 *   8   6 PA-AUTH-ORIG-DATE             X(06)
 *  14   6 PA-AUTH-ORIG-TIME             X(06)
 *  20  16 PA-CARD-NUM                   X(16)
 *  36   4 PA-AUTH-TYPE                  X(04)
 *  40   4 PA-CARD-EXPIRY-DATE           X(04)
 *  44   6 PA-MESSAGE-TYPE               X(06)
 *  50   6 PA-MESSAGE-SOURCE             X(06)
 *  56   6 PA-AUTH-ID-CODE               X(06)
 *  62   2 PA-AUTH-RESP-CODE             X(02)   88 PA-AUTH-APPROVED '00'
 *  64   4 PA-AUTH-RESP-REASON           X(04)
 *  68   6 PA-PROCESSING-CODE            9(06)
 *  74   7 PA-TRANSACTION-AMT            S9(10)V99 COMP-3
 *  81   7 PA-APPROVED-AMT               S9(10)V99 COMP-3
 *  88   4 PA-MERCHANT-CATAGORY-CODE     X(04)
 *  92   3 PA-ACQR-COUNTRY-CODE          X(03)
 *  95   2 PA-POS-ENTRY-MODE             9(02)
 *  97  15 PA-MERCHANT-ID                X(15)
 * 112  22 PA-MERCHANT-NAME              X(22)
 * 134  13 PA-MERCHANT-CITY              X(13)
 * 147   2 PA-MERCHANT-STATE             X(02)
 * 149   9 PA-MERCHANT-ZIP               X(09)
 * 158  15 PA-TRANSACTION-ID             X(15)
 * 173   1 PA-MATCH-STATUS               X(01)   88s P/D/E/M
 * 174   1 PA-AUTH-FRAUD                 X(01)   88s F/R
 * 175   8 PA-FRAUD-RPT-DATE             X(08)
 * 183  17 FILLER                        X(17)  (kept: GSAM output is byte-for-byte)
 * </pre>
 */
public record PendingAuthDetails(
        BigDecimal paAuthDate9c,
        BigDecimal paAuthTime9c,
        String paAuthOrigDate,
        String paAuthOrigTime,
        String paCardNum,
        String paAuthType,
        String paCardExpiryDate,
        String paMessageType,
        String paMessageSource,
        String paAuthIdCode,
        String paAuthRespCode,
        String paAuthRespReason,
        int paProcessingCode,
        BigDecimal paTransactionAmt,
        BigDecimal paApprovedAmt,
        String paMerchantCatagoryCode,
        String paAcqrCountryCode,
        int paPosEntryMode,
        String paMerchantId,
        String paMerchantName,
        String paMerchantCity,
        String paMerchantState,
        String paMerchantZip,
        String paTransactionId,
        String paMatchStatus,
        String paAuthFraud,
        String paFraudRptDate,
        String filler) {

    public static final int LENGTH = 200;
    /** PA-AUTHORIZATION-KEY length (PA-AUTH-DATE-9C + PA-AUTH-TIME-9C). */
    public static final int KEY_LENGTH = 8;

    /** 88-levels on PA-MATCH-STATUS. */
    public enum MatchStatus {
        PA_MATCH_PENDING("P"),
        PA_MATCH_AUTH_DECLINED("D"),
        PA_MATCH_PENDING_EXPIRED("E"),
        PA_MATCHED_WITH_TRAN("M");

        public final String value;

        MatchStatus(String value) {
            this.value = value;
        }

        public static Optional<MatchStatus> of(String code) {
            return Arrays.stream(values()).filter(m -> m.value.equals(code)).findFirst();
        }
    }

    /** 88-levels on PA-AUTH-FRAUD. */
    public enum AuthFraud {
        PA_FRAUD_CONFIRMED("F"),
        PA_FRAUD_REMOVED("R");

        public final String value;

        AuthFraud(String value) {
            this.value = value;
        }

        public static Optional<AuthFraud> of(String code) {
            return Arrays.stream(values()).filter(f -> f.value.equals(code)).findFirst();
        }
    }

    public PendingAuthDetails {
        paAuthOrigDate = CobolCodec.pic(paAuthOrigDate, 6);
        paAuthOrigTime = CobolCodec.pic(paAuthOrigTime, 6);
        paCardNum = CobolCodec.pic(paCardNum, 16);
        paAuthType = CobolCodec.pic(paAuthType, 4);
        paCardExpiryDate = CobolCodec.pic(paCardExpiryDate, 4);
        paMessageType = CobolCodec.pic(paMessageType, 6);
        paMessageSource = CobolCodec.pic(paMessageSource, 6);
        paAuthIdCode = CobolCodec.pic(paAuthIdCode, 6);
        paAuthRespCode = CobolCodec.pic(paAuthRespCode, 2);
        paAuthRespReason = CobolCodec.pic(paAuthRespReason, 4);
        paMerchantCatagoryCode = CobolCodec.pic(paMerchantCatagoryCode, 4);
        paAcqrCountryCode = CobolCodec.pic(paAcqrCountryCode, 3);
        paMerchantId = CobolCodec.pic(paMerchantId, 15);
        paMerchantName = CobolCodec.pic(paMerchantName, 22);
        paMerchantCity = CobolCodec.pic(paMerchantCity, 13);
        paMerchantState = CobolCodec.pic(paMerchantState, 2);
        paMerchantZip = CobolCodec.pic(paMerchantZip, 9);
        paTransactionId = CobolCodec.pic(paTransactionId, 15);
        paMatchStatus = CobolCodec.pic(paMatchStatus, 1);
        paAuthFraud = CobolCodec.pic(paAuthFraud, 1);
        paFraudRptDate = CobolCodec.pic(paFraudRptDate, 8);
        filler = CobolCodec.pic(filler, 17);
        paAuthDate9c = scaled(paAuthDate9c, 0);
        paAuthTime9c = scaled(paAuthTime9c, 0);
        paTransactionAmt = scaled(paTransactionAmt, 2);
        paApprovedAmt = scaled(paApprovedAmt, 2);
    }

    private static BigDecimal scaled(BigDecimal v, int scale) {
        return java.util.Objects.requireNonNull(v).setScale(scale, java.math.RoundingMode.DOWN);
    }

    /** 88 PA-AUTH-APPROVED VALUE '00'. */
    public boolean isPaAuthApproved() {
        return "00".equals(paAuthRespCode);
    }

    public Optional<MatchStatus> matchStatus() {
        return MatchStatus.of(paMatchStatus);
    }

    public Optional<AuthFraud> authFraud() {
        return AuthFraud.of(paAuthFraud);
    }

    public static PendingAuthDetails fromBytes(byte[] b, Charset cs) {
        CobolCodec.requireLength(b, LENGTH, "PENDING-AUTH-DETAILS");
        return new PendingAuthDetails(
                CobolCodec.decodePacked(b, 0, 5, 0),
                CobolCodec.decodePacked(b, 3, 9, 0),
                CobolCodec.decodeAlpha(b, 8, 6, cs),
                CobolCodec.decodeAlpha(b, 14, 6, cs),
                CobolCodec.decodeAlpha(b, 20, 16, cs),
                CobolCodec.decodeAlpha(b, 36, 4, cs),
                CobolCodec.decodeAlpha(b, 40, 4, cs),
                CobolCodec.decodeAlpha(b, 44, 6, cs),
                CobolCodec.decodeAlpha(b, 50, 6, cs),
                CobolCodec.decodeAlpha(b, 56, 6, cs),
                CobolCodec.decodeAlpha(b, 62, 2, cs),
                CobolCodec.decodeAlpha(b, 64, 4, cs),
                (int) CobolCodec.decodeZoned(b, 68, 6, cs),
                CobolCodec.decodePacked(b, 74, 12, 2),
                CobolCodec.decodePacked(b, 81, 12, 2),
                CobolCodec.decodeAlpha(b, 88, 4, cs),
                CobolCodec.decodeAlpha(b, 92, 3, cs),
                (int) CobolCodec.decodeZoned(b, 95, 2, cs),
                CobolCodec.decodeAlpha(b, 97, 15, cs),
                CobolCodec.decodeAlpha(b, 112, 22, cs),
                CobolCodec.decodeAlpha(b, 134, 13, cs),
                CobolCodec.decodeAlpha(b, 147, 2, cs),
                CobolCodec.decodeAlpha(b, 149, 9, cs),
                CobolCodec.decodeAlpha(b, 158, 15, cs),
                CobolCodec.decodeAlpha(b, 173, 1, cs),
                CobolCodec.decodeAlpha(b, 174, 1, cs),
                CobolCodec.decodeAlpha(b, 175, 8, cs),
                CobolCodec.decodeAlpha(b, 183, 17, cs));
    }

    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        CobolCodec.encodePacked(paAuthDate9c, b, 0, 5, 0);
        CobolCodec.encodePacked(paAuthTime9c, b, 3, 9, 0);
        CobolCodec.encodeAlpha(paAuthOrigDate, b, 8, 6, cs);
        CobolCodec.encodeAlpha(paAuthOrigTime, b, 14, 6, cs);
        CobolCodec.encodeAlpha(paCardNum, b, 20, 16, cs);
        CobolCodec.encodeAlpha(paAuthType, b, 36, 4, cs);
        CobolCodec.encodeAlpha(paCardExpiryDate, b, 40, 4, cs);
        CobolCodec.encodeAlpha(paMessageType, b, 44, 6, cs);
        CobolCodec.encodeAlpha(paMessageSource, b, 50, 6, cs);
        CobolCodec.encodeAlpha(paAuthIdCode, b, 56, 6, cs);
        CobolCodec.encodeAlpha(paAuthRespCode, b, 62, 2, cs);
        CobolCodec.encodeAlpha(paAuthRespReason, b, 64, 4, cs);
        CobolCodec.encodeZoned(paProcessingCode, b, 68, 6, cs);
        CobolCodec.encodePacked(paTransactionAmt, b, 74, 12, 2);
        CobolCodec.encodePacked(paApprovedAmt, b, 81, 12, 2);
        CobolCodec.encodeAlpha(paMerchantCatagoryCode, b, 88, 4, cs);
        CobolCodec.encodeAlpha(paAcqrCountryCode, b, 92, 3, cs);
        CobolCodec.encodeZoned(paPosEntryMode, b, 95, 2, cs);
        CobolCodec.encodeAlpha(paMerchantId, b, 97, 15, cs);
        CobolCodec.encodeAlpha(paMerchantName, b, 112, 22, cs);
        CobolCodec.encodeAlpha(paMerchantCity, b, 134, 13, cs);
        CobolCodec.encodeAlpha(paMerchantState, b, 147, 2, cs);
        CobolCodec.encodeAlpha(paMerchantZip, b, 149, 9, cs);
        CobolCodec.encodeAlpha(paTransactionId, b, 158, 15, cs);
        CobolCodec.encodeAlpha(paMatchStatus, b, 173, 1, cs);
        CobolCodec.encodeAlpha(paAuthFraud, b, 174, 1, cs);
        CobolCodec.encodeAlpha(paFraudRptDate, b, 175, 8, cs);
        CobolCodec.encodeAlpha(filler, b, 183, 17, cs);
        return b;
    }
}
