package com.carddemo.programs.paudblod.copybook;

import static com.carddemo.programs.paudblod.copybook.CobolFields.*;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Optional;

/**
 * Copybook CIPAUDTY: IMS segment PAUTDTL1 (pending authorization details, child), 200 bytes.
 * {@code 01 PENDING-AUTH-DETAILS} in PAUDBLOD. The first 8 bytes (PA-AUTHORIZATION-KEY) are the
 * PAUT9CTS sequence field.
 */
public record PendingAuthDetails(
        /* 10 PA-AUTH-DATE-9C          PIC S9(05) COMP-3     */ BigDecimal paAuthDate9c,
        /* 10 PA-AUTH-TIME-9C          PIC S9(09) COMP-3     */ BigDecimal paAuthTime9c,
        /* 05 PA-AUTH-ORIG-DATE        PIC  X(06)            */ String paAuthOrigDate,
        /* 05 PA-AUTH-ORIG-TIME        PIC  X(06)            */ String paAuthOrigTime,
        /* 05 PA-CARD-NUM              PIC  X(16)            */ String paCardNum,
        /* 05 PA-AUTH-TYPE             PIC  X(04)            */ String paAuthType,
        /* 05 PA-CARD-EXPIRY-DATE      PIC  X(04)            */ String paCardExpiryDate,
        /* 05 PA-MESSAGE-TYPE          PIC  X(06)            */ String paMessageType,
        /* 05 PA-MESSAGE-SOURCE        PIC  X(06)            */ String paMessageSource,
        /* 05 PA-AUTH-ID-CODE          PIC  X(06)            */ String paAuthIdCode,
        /* 05 PA-AUTH-RESP-CODE        PIC  X(02)            */ String paAuthRespCode,
        /* 05 PA-AUTH-RESP-REASON      PIC  X(04)            */ String paAuthRespReason,
        /* 05 PA-PROCESSING-CODE       PIC  9(06)            */ long paProcessingCode,
        /* 05 PA-TRANSACTION-AMT       PIC S9(10)V99 COMP-3  */ BigDecimal paTransactionAmt,
        /* 05 PA-APPROVED-AMT          PIC S9(10)V99 COMP-3  */ BigDecimal paApprovedAmt,
        /* 05 PA-MERCHANT-CATAGORY-CODE PIC X(04)            */ String paMerchantCatagoryCode,
        /* 05 PA-ACQR-COUNTRY-CODE     PIC  X(03)            */ String paAcqrCountryCode,
        /* 05 PA-POS-ENTRY-MODE        PIC  9(02)            */ int paPosEntryMode,
        /* 05 PA-MERCHANT-ID           PIC  X(15)            */ String paMerchantId,
        /* 05 PA-MERCHANT-NAME         PIC  X(22)            */ String paMerchantName,
        /* 05 PA-MERCHANT-CITY         PIC  X(13)            */ String paMerchantCity,
        /* 05 PA-MERCHANT-STATE        PIC  X(02)            */ String paMerchantState,
        /* 05 PA-MERCHANT-ZIP          PIC  X(09)            */ String paMerchantZip,
        /* 05 PA-TRANSACTION-ID        PIC  X(15)            */ String paTransactionId,
        /* 05 PA-MATCH-STATUS          PIC  X(01)            */ String paMatchStatus,
        /* 05 PA-AUTH-FRAUD            PIC  X(01)            */ String paAuthFraud,
        /* 05 PA-FRAUD-RPT-DATE        PIC  X(08)            */ String paFraudRptDate,
        /* 05 FILLER                   PIC  X(17)            */ String filler) {

    public static final int LENGTH = 200;
    /** Length of PA-AUTHORIZATION-KEY (PAUT9CTS, TYPE=C, 8 bytes). */
    public static final int KEY_LENGTH = 8;

    /** 88-levels on PA-MATCH-STATUS. */
    public enum MatchStatus {
        PA_MATCH_PENDING("P"),
        PA_MATCH_AUTH_DECLINED("D"),
        PA_MATCH_PENDING_EXPIRED("E"),
        PA_MATCHED_WITH_TRAN("M");

        private final String value;

        MatchStatus(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }

        public static Optional<MatchStatus> of(String v) {
            return Arrays.stream(values()).filter(m -> m.value.equals(v)).findFirst();
        }
    }

    /** 88-levels on PA-AUTH-FRAUD. */
    public enum AuthFraud {
        PA_FRAUD_CONFIRMED("F"),
        PA_FRAUD_REMOVED("R");

        private final String value;

        AuthFraud(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }

        public static Optional<AuthFraud> of(String v) {
            return Arrays.stream(values()).filter(m -> m.value.equals(v)).findFirst();
        }
    }

    public PendingAuthDetails {
        paAuthOrigDate = fixed(paAuthOrigDate, 6);
        paAuthOrigTime = fixed(paAuthOrigTime, 6);
        paCardNum = fixed(paCardNum, 16);
        paAuthType = fixed(paAuthType, 4);
        paCardExpiryDate = fixed(paCardExpiryDate, 4);
        paMessageType = fixed(paMessageType, 6);
        paMessageSource = fixed(paMessageSource, 6);
        paAuthIdCode = fixed(paAuthIdCode, 6);
        paAuthRespCode = fixed(paAuthRespCode, 2);
        paAuthRespReason = fixed(paAuthRespReason, 4);
        paMerchantCatagoryCode = fixed(paMerchantCatagoryCode, 4);
        paAcqrCountryCode = fixed(paAcqrCountryCode, 3);
        paMerchantId = fixed(paMerchantId, 15);
        paMerchantName = fixed(paMerchantName, 22);
        paMerchantCity = fixed(paMerchantCity, 13);
        paMerchantState = fixed(paMerchantState, 2);
        paMerchantZip = fixed(paMerchantZip, 9);
        paTransactionId = fixed(paTransactionId, 15);
        paMatchStatus = fixed(paMatchStatus, 1);
        paAuthFraud = fixed(paAuthFraud, 1);
        paFraudRptDate = fixed(paFraudRptDate, 8);
        filler = fixed(filler, 17);
    }

    /** 88 PA-AUTH-APPROVED VALUE '00'. */
    public boolean isPaAuthApproved() {
        return "00".equals(paAuthRespCode);
    }

    public boolean isPaMatchPending() {
        return MatchStatus.PA_MATCH_PENDING.value.equals(paMatchStatus);
    }

    public boolean isPaMatchAuthDeclined() {
        return MatchStatus.PA_MATCH_AUTH_DECLINED.value.equals(paMatchStatus);
    }

    public boolean isPaMatchPendingExpired() {
        return MatchStatus.PA_MATCH_PENDING_EXPIRED.value.equals(paMatchStatus);
    }

    public boolean isPaMatchedWithTran() {
        return MatchStatus.PA_MATCHED_WITH_TRAN.value.equals(paMatchStatus);
    }

    public boolean isPaFraudConfirmed() {
        return AuthFraud.PA_FRAUD_CONFIRMED.value.equals(paAuthFraud);
    }

    public boolean isPaFraudRemoved() {
        return AuthFraud.PA_FRAUD_REMOVED.value.equals(paAuthFraud);
    }

    public static PendingAuthDetails fromBytes(byte[] b, Charset cs) {
        if (b.length != LENGTH) {
            throw new IllegalArgumentException("PAUTDTL1 must be " + LENGTH + " bytes, got " + b.length);
        }
        return new PendingAuthDetails(
                decodePacked(b, 0, 3, 0),
                decodePacked(b, 3, 5, 0),
                decodeText(b, 8, 6, cs),
                decodeText(b, 14, 6, cs),
                decodeText(b, 20, 16, cs),
                decodeText(b, 36, 4, cs),
                decodeText(b, 40, 4, cs),
                decodeText(b, 44, 6, cs),
                decodeText(b, 50, 6, cs),
                decodeText(b, 56, 6, cs),
                decodeText(b, 62, 2, cs),
                decodeText(b, 64, 4, cs),
                decodeZoned(b, 68, 6, cs),
                decodePacked(b, 74, 7, 2),
                decodePacked(b, 81, 7, 2),
                decodeText(b, 88, 4, cs),
                decodeText(b, 92, 3, cs),
                (int) decodeZoned(b, 95, 2, cs),
                decodeText(b, 97, 15, cs),
                decodeText(b, 112, 22, cs),
                decodeText(b, 134, 13, cs),
                decodeText(b, 147, 2, cs),
                decodeText(b, 149, 9, cs),
                decodeText(b, 158, 15, cs),
                decodeText(b, 173, 1, cs),
                decodeText(b, 174, 1, cs),
                decodeText(b, 175, 8, cs),
                decodeText(b, 183, 17, cs));
    }

    public byte[] toBytes(Charset cs) {
        byte[] b = new byte[LENGTH];
        encodePacked(paAuthDate9c, 5, 0, b, 0);
        encodePacked(paAuthTime9c, 9, 0, b, 3);
        encodeText(paAuthOrigDate, 6, b, 8, cs);
        encodeText(paAuthOrigTime, 6, b, 14, cs);
        encodeText(paCardNum, 16, b, 20, cs);
        encodeText(paAuthType, 4, b, 36, cs);
        encodeText(paCardExpiryDate, 4, b, 40, cs);
        encodeText(paMessageType, 6, b, 44, cs);
        encodeText(paMessageSource, 6, b, 50, cs);
        encodeText(paAuthIdCode, 6, b, 56, cs);
        encodeText(paAuthRespCode, 2, b, 62, cs);
        encodeText(paAuthRespReason, 4, b, 64, cs);
        encodeZoned(paProcessingCode, 6, b, 68, cs);
        encodePacked(paTransactionAmt, 12, 2, b, 74);
        encodePacked(paApprovedAmt, 12, 2, b, 81);
        encodeText(paMerchantCatagoryCode, 4, b, 88, cs);
        encodeText(paAcqrCountryCode, 3, b, 92, cs);
        encodeZoned(paPosEntryMode, 2, b, 95, cs);
        encodeText(paMerchantId, 15, b, 97, cs);
        encodeText(paMerchantName, 22, b, 112, cs);
        encodeText(paMerchantCity, 13, b, 134, cs);
        encodeText(paMerchantState, 2, b, 147, cs);
        encodeText(paMerchantZip, 9, b, 149, cs);
        encodeText(paTransactionId, 15, b, 158, cs);
        encodeText(paMatchStatus, 1, b, 173, cs);
        encodeText(paAuthFraud, 1, b, 174, cs);
        encodeText(paFraudRptDate, 8, b, 175, cs);
        encodeText(filler, 17, b, 183, cs);
        return b;
    }
}
