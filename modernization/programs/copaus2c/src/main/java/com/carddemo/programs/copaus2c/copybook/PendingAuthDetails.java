package com.carddemo.programs.copaus2c.copybook;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Optional;

/**
 * CIPAUDTY - IMS segment PAUTDTL1, pending authorization details (200 bytes).
 * Every field of the copybook is kept, including the trailing FILLER X(17).
 */
public record PendingAuthDetails(
        long authDate9C,              // PA-AUTH-DATE-9C            PIC S9(05) COMP-3   (PA-AUTHORIZATION-KEY)
        long authTime9C,              // PA-AUTH-TIME-9C            PIC S9(09) COMP-3   (PA-AUTHORIZATION-KEY)
        String authOrigDate,          // PA-AUTH-ORIG-DATE          PIC X(06)  YYMMDD
        String authOrigTime,          // PA-AUTH-ORIG-TIME          PIC X(06)
        String cardNum,               // PA-CARD-NUM                PIC X(16)
        String authType,              // PA-AUTH-TYPE               PIC X(04)
        String cardExpiryDate,        // PA-CARD-EXPIRY-DATE        PIC X(04)
        String messageType,           // PA-MESSAGE-TYPE            PIC X(06)
        String messageSource,         // PA-MESSAGE-SOURCE          PIC X(06)
        String authIdCode,            // PA-AUTH-ID-CODE            PIC X(06)
        String authRespCode,          // PA-AUTH-RESP-CODE          PIC X(02)
        String authRespReason,        // PA-AUTH-RESP-REASON        PIC X(04)
        long processingCode,          // PA-PROCESSING-CODE         PIC 9(06)
        BigDecimal transactionAmt,    // PA-TRANSACTION-AMT         PIC S9(10)V99 COMP-3
        BigDecimal approvedAmt,       // PA-APPROVED-AMT            PIC S9(10)V99 COMP-3
        String merchantCatagoryCode,  // PA-MERCHANT-CATAGORY-CODE  PIC X(04)
        String acqrCountryCode,       // PA-ACQR-COUNTRY-CODE       PIC X(03)
        int posEntryMode,             // PA-POS-ENTRY-MODE          PIC 9(02)
        String merchantId,            // PA-MERCHANT-ID             PIC X(15)
        String merchantName,          // PA-MERCHANT-NAME           PIC X(22)
        String merchantCity,          // PA-MERCHANT-CITY           PIC X(13)
        String merchantState,         // PA-MERCHANT-STATE          PIC X(02)
        String merchantZip,           // PA-MERCHANT-ZIP            PIC X(09)
        String transactionId,         // PA-TRANSACTION-ID          PIC X(15)
        String matchStatus,           // PA-MATCH-STATUS            PIC X(01)
        String authFraud,             // PA-AUTH-FRAUD              PIC X(01)
        String fraudRptDate,          // PA-FRAUD-RPT-DATE          PIC X(08)
        String filler) {              // FILLER                     PIC X(17)

    public static final int LENGTH = 200;
    public static final int MERCHANT_NAME_LENGTH = 22;

    public PendingAuthDetails {
        authDate9C = Pic.signed(authDate9C, 5);
        authTime9C = Pic.signed(authTime9C, 9);
        authOrigDate = Pic.x(authOrigDate, 6);
        authOrigTime = Pic.x(authOrigTime, 6);
        cardNum = Pic.x(cardNum, 16);
        authType = Pic.x(authType, 4);
        cardExpiryDate = Pic.x(cardExpiryDate, 4);
        messageType = Pic.x(messageType, 6);
        messageSource = Pic.x(messageSource, 6);
        authIdCode = Pic.x(authIdCode, 6);
        authRespCode = Pic.x(authRespCode, 2);
        authRespReason = Pic.x(authRespReason, 4);
        processingCode = Pic.unsigned(processingCode, 6);
        transactionAmt = Pic.decimal(transactionAmt == null ? BigDecimal.ZERO : transactionAmt, 10, 2);
        approvedAmt = Pic.decimal(approvedAmt == null ? BigDecimal.ZERO : approvedAmt, 10, 2);
        merchantCatagoryCode = Pic.x(merchantCatagoryCode, 4);
        acqrCountryCode = Pic.x(acqrCountryCode, 3);
        posEntryMode = (int) Pic.unsigned(posEntryMode, 2);
        merchantId = Pic.x(merchantId, 15);
        merchantName = Pic.x(merchantName, MERCHANT_NAME_LENGTH);
        merchantCity = Pic.x(merchantCity, 13);
        merchantState = Pic.x(merchantState, 2);
        merchantZip = Pic.x(merchantZip, 9);
        transactionId = Pic.x(transactionId, 15);
        matchStatus = Pic.x(matchStatus, 1);
        authFraud = Pic.x(authFraud, 1);
        fraudRptDate = Pic.x(fraudRptDate, 8);
        filler = Pic.x(filler, 17);
    }

    /** 88 PA-AUTH-APPROVED VALUE '00'. */
    public boolean isAuthApproved() {
        return "00".equals(authRespCode);
    }

    public Optional<MatchStatus> matchStatusCode() {
        return MatchStatus.of(matchStatus);
    }

    public Optional<AuthFraud> authFraudCode() {
        return AuthFraud.of(authFraud);
    }

    public PendingAuthDetails withFraudRptDate(String newFraudRptDate) {
        return new PendingAuthDetails(authDate9C, authTime9C, authOrigDate, authOrigTime, cardNum, authType,
                cardExpiryDate, messageType, messageSource, authIdCode, authRespCode, authRespReason,
                processingCode, transactionAmt, approvedAmt, merchantCatagoryCode, acqrCountryCode, posEntryMode,
                merchantId, merchantName, merchantCity, merchantState, merchantZip, transactionId, matchStatus,
                authFraud, newFraudRptDate, filler);
    }

    /** PA-MATCH-STATUS 88-levels. */
    public enum MatchStatus {
        PA_MATCH_PENDING("P"),
        PA_MATCH_AUTH_DECLINED("D"),
        PA_MATCH_PENDING_EXPIRED("E"),
        PA_MATCHED_WITH_TRAN("M");

        public final String value;

        MatchStatus(String value) {
            this.value = value;
        }

        public static Optional<MatchStatus> of(String v) {
            return Arrays.stream(values()).filter(s -> s.value.equals(v)).findFirst();
        }
    }

    /** PA-AUTH-FRAUD 88-levels. */
    public enum AuthFraud {
        PA_FRAUD_CONFIRMED("F"),
        PA_FRAUD_REMOVED("R");

        public final String value;

        AuthFraud(String value) {
            this.value = value;
        }

        public static Optional<AuthFraud> of(String v) {
            return Arrays.stream(values()).filter(s -> s.value.equals(v)).findFirst();
        }
    }

    /** Decode the 200-byte EBCDIC segment image starting at {@code offset}. */
    public static PendingAuthDetails fromEbcdic(byte[] rec, int offset) {
        return new PendingAuthDetails(
                Ebcdic.unpack(rec, offset, 3, 0).longValueExact(),
                Ebcdic.unpack(rec, offset + 3, 5, 0).longValueExact(),
                Ebcdic.text(rec, offset + 8, 6),
                Ebcdic.text(rec, offset + 14, 6),
                Ebcdic.text(rec, offset + 20, 16),
                Ebcdic.text(rec, offset + 36, 4),
                Ebcdic.text(rec, offset + 40, 4),
                Ebcdic.text(rec, offset + 44, 6),
                Ebcdic.text(rec, offset + 50, 6),
                Ebcdic.text(rec, offset + 56, 6),
                Ebcdic.text(rec, offset + 62, 2),
                Ebcdic.text(rec, offset + 64, 4),
                Long.parseLong(Ebcdic.text(rec, offset + 68, 6)),
                Ebcdic.unpack(rec, offset + 74, 7, 2),
                Ebcdic.unpack(rec, offset + 81, 7, 2),
                Ebcdic.text(rec, offset + 88, 4),
                Ebcdic.text(rec, offset + 92, 3),
                Integer.parseInt(Ebcdic.text(rec, offset + 95, 2)),
                Ebcdic.text(rec, offset + 97, 15),
                Ebcdic.text(rec, offset + 112, 22),
                Ebcdic.text(rec, offset + 134, 13),
                Ebcdic.text(rec, offset + 147, 2),
                Ebcdic.text(rec, offset + 149, 9),
                Ebcdic.text(rec, offset + 158, 15),
                Ebcdic.text(rec, offset + 173, 1),
                Ebcdic.text(rec, offset + 174, 1),
                Ebcdic.text(rec, offset + 175, 8),
                Ebcdic.text(rec, offset + 183, 17));
    }

    /** Encode into a 200-byte EBCDIC segment image at {@code offset}. */
    public void toEbcdic(byte[] rec, int offset) {
        Ebcdic.pack(rec, offset, 3, 0, BigDecimal.valueOf(authDate9C));
        Ebcdic.pack(rec, offset + 3, 5, 0, BigDecimal.valueOf(authTime9C));
        Ebcdic.putText(rec, offset + 8, 6, authOrigDate);
        Ebcdic.putText(rec, offset + 14, 6, authOrigTime);
        Ebcdic.putText(rec, offset + 20, 16, cardNum);
        Ebcdic.putText(rec, offset + 36, 4, authType);
        Ebcdic.putText(rec, offset + 40, 4, cardExpiryDate);
        Ebcdic.putText(rec, offset + 44, 6, messageType);
        Ebcdic.putText(rec, offset + 50, 6, messageSource);
        Ebcdic.putText(rec, offset + 56, 6, authIdCode);
        Ebcdic.putText(rec, offset + 62, 2, authRespCode);
        Ebcdic.putText(rec, offset + 64, 4, authRespReason);
        Ebcdic.putText(rec, offset + 68, 6, Pic.display9(processingCode, 6));
        Ebcdic.pack(rec, offset + 74, 7, 2, transactionAmt);
        Ebcdic.pack(rec, offset + 81, 7, 2, approvedAmt);
        Ebcdic.putText(rec, offset + 88, 4, merchantCatagoryCode);
        Ebcdic.putText(rec, offset + 92, 3, acqrCountryCode);
        Ebcdic.putText(rec, offset + 95, 2, Pic.display9(posEntryMode, 2));
        Ebcdic.putText(rec, offset + 97, 15, merchantId);
        Ebcdic.putText(rec, offset + 112, 22, merchantName);
        Ebcdic.putText(rec, offset + 134, 13, merchantCity);
        Ebcdic.putText(rec, offset + 147, 2, merchantState);
        Ebcdic.putText(rec, offset + 149, 9, merchantZip);
        Ebcdic.putText(rec, offset + 158, 15, transactionId);
        Ebcdic.putText(rec, offset + 173, 1, matchStatus);
        Ebcdic.putText(rec, offset + 174, 1, authFraud);
        Ebcdic.putText(rec, offset + 175, 8, fraudRptDate);
        Ebcdic.putText(rec, offset + 183, 17, filler);
    }
}
