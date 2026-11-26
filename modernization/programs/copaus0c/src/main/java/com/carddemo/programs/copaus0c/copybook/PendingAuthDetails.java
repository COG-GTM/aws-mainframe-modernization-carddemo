package com.carddemo.programs.copaus0c.copybook;

import com.carddemo.programs.copaus0c.cobol.PackedDecimal;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/** CIPAUDTY - PENDING-AUTH-DETAILS, IMS child segment PAUTDTL1 (200 bytes). FILLER X(17) dropped. */
public record PendingAuthDetails(
        long paAuthDate9c,               // PA-AUTH-DATE-9C (PA-AUTHORIZATION-KEY) PIC S9(05) COMP-3
        long paAuthTime9c,               // PA-AUTH-TIME-9C (PA-AUTHORIZATION-KEY) PIC S9(09) COMP-3
        String paAuthOrigDate,           // PA-AUTH-ORIG-DATE         PIC X(06)  YYMMDD
        String paAuthOrigTime,           // PA-AUTH-ORIG-TIME         PIC X(06)  HHMMSS
        String paCardNum,                // PA-CARD-NUM               PIC X(16)
        String paAuthType,               // PA-AUTH-TYPE              PIC X(04)
        String paCardExpiryDate,         // PA-CARD-EXPIRY-DATE       PIC X(04)
        String paMessageType,            // PA-MESSAGE-TYPE           PIC X(06)
        String paMessageSource,          // PA-MESSAGE-SOURCE         PIC X(06)
        String paAuthIdCode,             // PA-AUTH-ID-CODE           PIC X(06)
        String paAuthRespCode,           // PA-AUTH-RESP-CODE         PIC X(02)
        String paAuthRespReason,         // PA-AUTH-RESP-REASON       PIC X(04)
        String paProcessingCode,         // PA-PROCESSING-CODE        PIC 9(06)
        BigDecimal paTransactionAmt,     // PA-TRANSACTION-AMT        PIC S9(10)V99 COMP-3
        BigDecimal paApprovedAmt,        // PA-APPROVED-AMT           PIC S9(10)V99 COMP-3
        String paMerchantCatagoryCode,   // PA-MERCHANT-CATAGORY-CODE PIC X(04)
        String paAcqrCountryCode,        // PA-ACQR-COUNTRY-CODE      PIC X(03)
        String paPosEntryMode,           // PA-POS-ENTRY-MODE         PIC 9(02)
        String paMerchantId,             // PA-MERCHANT-ID            PIC X(15)
        String paMerchantName,           // PA-MERCHANT-NAME          PIC X(22)
        String paMerchantCity,           // PA-MERCHANT-CITY          PIC X(13)
        String paMerchantState,          // PA-MERCHANT-STATE         PIC X(02)
        String paMerchantZip,            // PA-MERCHANT-ZIP           PIC X(09)
        String paTransactionId,          // PA-TRANSACTION-ID         PIC X(15)
        String paMatchStatus,            // PA-MATCH-STATUS           PIC X(01)
        String paAuthFraud,              // PA-AUTH-FRAUD             PIC X(01)
        String paFraudRptDate) {         // PA-FRAUD-RPT-DATE         PIC X(08)

    public static final int SEGMENT_LENGTH = 200;

    /** 88-levels on PA-MATCH-STATUS. */
    public enum MatchStatus {
        PA_MATCH_PENDING("P"), PA_MATCH_AUTH_DECLINED("D"), PA_MATCH_PENDING_EXPIRED("E"), PA_MATCHED_WITH_TRAN("M");

        public final String value;

        MatchStatus(String value) {
            this.value = value;
        }
    }

    /** 88-levels on PA-AUTH-FRAUD. */
    public enum AuthFraud {
        PA_FRAUD_CONFIRMED("F"), PA_FRAUD_REMOVED("R");

        public final String value;

        AuthFraud(String value) {
            this.value = value;
        }
    }

    /** 88 PA-AUTH-APPROVED VALUE '00'. */
    public boolean isPaAuthApproved() {
        return "00".equals(paAuthRespCode);
    }

    public boolean is(MatchStatus status) {
        return status.value.equals(paMatchStatus);
    }

    public boolean is(AuthFraud fraud) {
        return fraud.value.equals(paAuthFraud);
    }

    /**
     * PA-AUTHORIZATION-KEY (PAUT9CTS) as its 8-byte storage image (3+5 packed bytes), carried in a
     * String with one ISO-8859-1 char per byte so it can live in PIC X(08) COMMAREA fields.
     */
    public String authorizationKey() {
        return authorizationKey(paAuthDate9c, paAuthTime9c);
    }

    public static String authorizationKey(long authDate9c, long authTime9c) {
        byte[] key = new byte[8];
        System.arraycopy(PackedDecimal.encode(authDate9c, 5), 0, key, 0, 3);
        System.arraycopy(PackedDecimal.encode(authTime9c, 9), 0, key, 3, 5);
        return new String(key, StandardCharsets.ISO_8859_1);
    }

    public static PendingAuthDetails decode(byte[] seg, int off, Charset ebcdic) {
        return new PendingAuthDetails(
                PackedDecimal.decodeLong(seg, off, 5),
                PackedDecimal.decodeLong(seg, off + 3, 9),
                s(seg, off + 8, 6, ebcdic), s(seg, off + 14, 6, ebcdic), s(seg, off + 20, 16, ebcdic),
                s(seg, off + 36, 4, ebcdic), s(seg, off + 40, 4, ebcdic), s(seg, off + 44, 6, ebcdic),
                s(seg, off + 50, 6, ebcdic), s(seg, off + 56, 6, ebcdic), s(seg, off + 62, 2, ebcdic),
                s(seg, off + 64, 4, ebcdic), s(seg, off + 68, 6, ebcdic),
                PackedDecimal.decode(seg, off + 74, 12, 2),
                PackedDecimal.decode(seg, off + 81, 12, 2),
                s(seg, off + 88, 4, ebcdic), s(seg, off + 92, 3, ebcdic), s(seg, off + 95, 2, ebcdic),
                s(seg, off + 97, 15, ebcdic), s(seg, off + 112, 22, ebcdic), s(seg, off + 134, 13, ebcdic),
                s(seg, off + 147, 2, ebcdic), s(seg, off + 149, 9, ebcdic), s(seg, off + 158, 15, ebcdic),
                s(seg, off + 173, 1, ebcdic), s(seg, off + 174, 1, ebcdic), s(seg, off + 175, 8, ebcdic));
    }

    private static String s(byte[] b, int off, int len, Charset cs) {
        return new String(b, off, len, cs);
    }
}
