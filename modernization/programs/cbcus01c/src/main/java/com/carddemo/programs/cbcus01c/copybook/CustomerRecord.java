package com.carddemo.programs.cbcus01c.copybook;

import java.util.Objects;

/**
 * Copybook CVCUS01Y - 01 CUSTOMER-RECORD (RECLN 500).
 *
 * <p>Fixed-width layout, byte offsets are 0-based:
 * <pre>
 * CUST-ID                    PIC 9(09)   0
 * CUST-FIRST-NAME            PIC X(25)   9
 * CUST-MIDDLE-NAME           PIC X(25)  34
 * CUST-LAST-NAME             PIC X(25)  59
 * CUST-ADDR-LINE-1           PIC X(50)  84
 * CUST-ADDR-LINE-2           PIC X(50) 134
 * CUST-ADDR-LINE-3           PIC X(50) 184
 * CUST-ADDR-STATE-CD         PIC X(02) 234
 * CUST-ADDR-COUNTRY-CD       PIC X(03) 236
 * CUST-ADDR-ZIP              PIC X(10) 239
 * CUST-PHONE-NUM-1           PIC X(15) 249
 * CUST-PHONE-NUM-2           PIC X(15) 264
 * CUST-SSN                   PIC 9(09) 279
 * CUST-GOVT-ISSUED-ID        PIC X(20) 288
 * CUST-DOB-YYYY-MM-DD        PIC X(10) 308
 * CUST-EFT-ACCOUNT-ID        PIC X(10) 318
 * CUST-PRI-CARD-HOLDER-IND   PIC X(01) 328
 * CUST-FICO-CREDIT-SCORE     PIC 9(03) 329
 * FILLER                     PIC X(168) 332
 * </pre>
 * The copybook defines no 88-levels. Alphanumeric fields keep their trailing spaces so the
 * record round-trips byte-for-byte; the FILLER is kept for wire compatibility.
 */
public record CustomerRecord(
        long custId,
        String custFirstName,
        String custMiddleName,
        String custLastName,
        String custAddrLine1,
        String custAddrLine2,
        String custAddrLine3,
        String custAddrStateCd,
        String custAddrCountryCd,
        String custAddrZip,
        String custPhoneNum1,
        String custPhoneNum2,
        long custSsn,
        String custGovtIssuedId,
        String custDobYyyyMmDd,
        String custEftAccountId,
        String custPriCardHolderInd,
        int custFicoCreditScore,
        String filler) {

    public static final int RECORD_LENGTH = 500;

    public static final int CUST_ID_LEN = 9;
    public static final int CUST_FIRST_NAME_LEN = 25;
    public static final int CUST_MIDDLE_NAME_LEN = 25;
    public static final int CUST_LAST_NAME_LEN = 25;
    public static final int CUST_ADDR_LINE_LEN = 50;
    public static final int CUST_ADDR_STATE_CD_LEN = 2;
    public static final int CUST_ADDR_COUNTRY_CD_LEN = 3;
    public static final int CUST_ADDR_ZIP_LEN = 10;
    public static final int CUST_PHONE_NUM_LEN = 15;
    public static final int CUST_SSN_LEN = 9;
    public static final int CUST_GOVT_ISSUED_ID_LEN = 20;
    public static final int CUST_DOB_LEN = 10;
    public static final int CUST_EFT_ACCOUNT_ID_LEN = 10;
    public static final int CUST_PRI_CARD_HOLDER_IND_LEN = 1;
    public static final int CUST_FICO_CREDIT_SCORE_LEN = 3;
    public static final int FILLER_LEN = 168;

    public CustomerRecord {
        requireUnsigned(custId, CUST_ID_LEN, "CUST-ID");
        requireUnsigned(custSsn, CUST_SSN_LEN, "CUST-SSN");
        requireUnsigned(custFicoCreditScore, CUST_FICO_CREDIT_SCORE_LEN, "CUST-FICO-CREDIT-SCORE");
        custFirstName = pic(custFirstName, CUST_FIRST_NAME_LEN);
        custMiddleName = pic(custMiddleName, CUST_MIDDLE_NAME_LEN);
        custLastName = pic(custLastName, CUST_LAST_NAME_LEN);
        custAddrLine1 = pic(custAddrLine1, CUST_ADDR_LINE_LEN);
        custAddrLine2 = pic(custAddrLine2, CUST_ADDR_LINE_LEN);
        custAddrLine3 = pic(custAddrLine3, CUST_ADDR_LINE_LEN);
        custAddrStateCd = pic(custAddrStateCd, CUST_ADDR_STATE_CD_LEN);
        custAddrCountryCd = pic(custAddrCountryCd, CUST_ADDR_COUNTRY_CD_LEN);
        custAddrZip = pic(custAddrZip, CUST_ADDR_ZIP_LEN);
        custPhoneNum1 = pic(custPhoneNum1, CUST_PHONE_NUM_LEN);
        custPhoneNum2 = pic(custPhoneNum2, CUST_PHONE_NUM_LEN);
        custGovtIssuedId = pic(custGovtIssuedId, CUST_GOVT_ISSUED_ID_LEN);
        custDobYyyyMmDd = pic(custDobYyyyMmDd, CUST_DOB_LEN);
        custEftAccountId = pic(custEftAccountId, CUST_EFT_ACCOUNT_ID_LEN);
        custPriCardHolderInd = pic(custPriCardHolderInd, CUST_PRI_CARD_HOLDER_IND_LEN);
        filler = pic(filler, FILLER_LEN);
    }

    /** Parses a 500-byte CUSTOMER-RECORD image. Shorter input is space padded, longer is truncated (group MOVE). */
    public static CustomerRecord fromFixedWidth(String image) {
        Objects.requireNonNull(image, "image");
        Cursor c = new Cursor(pic(image, RECORD_LENGTH));
        return new CustomerRecord(
                c.unsigned(CUST_ID_LEN, "CUST-ID"),
                c.alpha(CUST_FIRST_NAME_LEN),
                c.alpha(CUST_MIDDLE_NAME_LEN),
                c.alpha(CUST_LAST_NAME_LEN),
                c.alpha(CUST_ADDR_LINE_LEN),
                c.alpha(CUST_ADDR_LINE_LEN),
                c.alpha(CUST_ADDR_LINE_LEN),
                c.alpha(CUST_ADDR_STATE_CD_LEN),
                c.alpha(CUST_ADDR_COUNTRY_CD_LEN),
                c.alpha(CUST_ADDR_ZIP_LEN),
                c.alpha(CUST_PHONE_NUM_LEN),
                c.alpha(CUST_PHONE_NUM_LEN),
                c.unsigned(CUST_SSN_LEN, "CUST-SSN"),
                c.alpha(CUST_GOVT_ISSUED_ID_LEN),
                c.alpha(CUST_DOB_LEN),
                c.alpha(CUST_EFT_ACCOUNT_ID_LEN),
                c.alpha(CUST_PRI_CARD_HOLDER_IND_LEN),
                (int) c.unsigned(CUST_FICO_CREDIT_SCORE_LEN, "CUST-FICO-CREDIT-SCORE"),
                c.alpha(FILLER_LEN));
    }

    /** Serialises back to the 500-byte image (PIC 9 zero-filled, PIC X space-padded). */
    public String toFixedWidth() {
        StringBuilder sb = new StringBuilder(RECORD_LENGTH);
        sb.append(zeroPad(custId, CUST_ID_LEN))
                .append(custFirstName).append(custMiddleName).append(custLastName)
                .append(custAddrLine1).append(custAddrLine2).append(custAddrLine3)
                .append(custAddrStateCd).append(custAddrCountryCd).append(custAddrZip)
                .append(custPhoneNum1).append(custPhoneNum2)
                .append(zeroPad(custSsn, CUST_SSN_LEN))
                .append(custGovtIssuedId).append(custDobYyyyMmDd).append(custEftAccountId)
                .append(custPriCardHolderInd)
                .append(zeroPad(custFicoCreditScore, CUST_FICO_CREDIT_SCORE_LEN))
                .append(filler);
        return sb.toString();
    }

    /** MOVE alphanumeric to PIC X(n): left-justify, pad with spaces or truncate on the right. */
    public static String pic(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    private static String zeroPad(long value, int length) {
        String s = Long.toString(value);
        return "0".repeat(length - s.length()) + s;
    }

    private static void requireUnsigned(long value, int digits, String name) {
        if (value < 0 || Long.toString(value).length() > digits) {
            throw new IllegalArgumentException(name + " must fit PIC 9(" + digits + "): " + value);
        }
    }

    private static final class Cursor {
        private final String image;
        private int pos;

        Cursor(String image) {
            this.image = image;
        }

        String alpha(int len) {
            String s = image.substring(pos, pos + len);
            pos += len;
            return s;
        }

        long unsigned(int len, String name) {
            String s = alpha(len);
            for (int i = 0; i < s.length(); i++) {
                if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                    throw new IllegalArgumentException(name + " is not NUMERIC: '" + s + "'");
                }
            }
            return Long.parseLong(s);
        }
    }
}
