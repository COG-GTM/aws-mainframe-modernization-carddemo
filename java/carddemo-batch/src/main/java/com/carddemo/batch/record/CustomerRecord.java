package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

/**
 * Copybook {@code CVCUS01Y} - {@code CUSTOMER-RECORD} (RECLN 500), the CUSTFILE KSDS record (key CUST-ID).
 * <pre>
 * 05  CUST-ID                     PIC 9(09).
 * 05  CUST-FIRST-NAME             PIC X(25).
 * 05  CUST-MIDDLE-NAME            PIC X(25).
 * 05  CUST-LAST-NAME              PIC X(25).
 * 05  CUST-ADDR-LINE-1            PIC X(50).
 * 05  CUST-ADDR-LINE-2            PIC X(50).
 * 05  CUST-ADDR-LINE-3            PIC X(50).
 * 05  CUST-ADDR-STATE-CD          PIC X(02).
 * 05  CUST-ADDR-COUNTRY-CD        PIC X(03).
 * 05  CUST-ADDR-ZIP               PIC X(10).
 * 05  CUST-PHONE-NUM-1            PIC X(15).
 * 05  CUST-PHONE-NUM-2            PIC X(15).
 * 05  CUST-SSN                    PIC 9(09).
 * 05  CUST-GOVT-ISSUED-ID         PIC X(20).
 * 05  CUST-DOB-YYYY-MM-DD         PIC X(10).
 * 05  CUST-EFT-ACCOUNT-ID         PIC X(10).
 * 05  CUST-PRI-CARD-HOLDER-IND    PIC X(01).
 * 05  CUST-FICO-CREDIT-SCORE      PIC 9(03).
 * 05  FILLER                      PIC X(168).
 * </pre>
 */
public final class CustomerRecord extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("CUSTOMER-RECORD");
    public static final Field CUST_ID = B.unsigned("CUST-ID", 9);
    public static final Field CUST_FIRST_NAME = B.text("CUST-FIRST-NAME", 25);
    public static final Field CUST_MIDDLE_NAME = B.text("CUST-MIDDLE-NAME", 25);
    public static final Field CUST_LAST_NAME = B.text("CUST-LAST-NAME", 25);
    public static final Field CUST_ADDR_LINE_1 = B.text("CUST-ADDR-LINE-1", 50);
    public static final Field CUST_ADDR_LINE_2 = B.text("CUST-ADDR-LINE-2", 50);
    public static final Field CUST_ADDR_LINE_3 = B.text("CUST-ADDR-LINE-3", 50);
    public static final Field CUST_ADDR_STATE_CD = B.text("CUST-ADDR-STATE-CD", 2);
    public static final Field CUST_ADDR_COUNTRY_CD = B.text("CUST-ADDR-COUNTRY-CD", 3);
    public static final Field CUST_ADDR_ZIP = B.text("CUST-ADDR-ZIP", 10);
    public static final Field CUST_PHONE_NUM_1 = B.text("CUST-PHONE-NUM-1", 15);
    public static final Field CUST_PHONE_NUM_2 = B.text("CUST-PHONE-NUM-2", 15);
    public static final Field CUST_SSN = B.unsigned("CUST-SSN", 9);
    public static final Field CUST_GOVT_ISSUED_ID = B.text("CUST-GOVT-ISSUED-ID", 20);
    public static final Field CUST_DOB_YYYY_MM_DD = B.text("CUST-DOB-YYYY-MM-DD", 10);
    public static final Field CUST_EFT_ACCOUNT_ID = B.text("CUST-EFT-ACCOUNT-ID", 10);
    public static final Field CUST_PRI_CARD_HOLDER_IND = B.text("CUST-PRI-CARD-HOLDER-IND", 1);
    public static final Field CUST_FICO_CREDIT_SCORE = B.unsigned("CUST-FICO-CREDIT-SCORE", 3);
    public static final Field FILLER = B.filler(168);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public CustomerRecord() {
        super(LAYOUT);
    }

    private CustomerRecord(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static CustomerRecord decode(byte[] raw) {
        return new CustomerRecord(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    /** COBOL {@code READ ... INTO CUSTOMER-RECORD}: replace the whole buffer. */
    public void moveFrom(byte[] raw) {
        System.arraycopy(FixedWidth.decode(LAYOUT, raw), 0, data, 0, data.length);
    }

    public long custId() {
        return FixedWidth.unsigned(data, CUST_ID);
    }

    public void setCustId(long v) {
        FixedWidth.setUnsigned(data, CUST_ID, v);
    }

    public String custFirstName() {
        return FixedWidth.text(data, CUST_FIRST_NAME);
    }

    public void setCustFirstName(String v) {
        FixedWidth.setText(data, CUST_FIRST_NAME, v);
    }

    public String custMiddleName() {
        return FixedWidth.text(data, CUST_MIDDLE_NAME);
    }

    public void setCustMiddleName(String v) {
        FixedWidth.setText(data, CUST_MIDDLE_NAME, v);
    }

    public String custLastName() {
        return FixedWidth.text(data, CUST_LAST_NAME);
    }

    public void setCustLastName(String v) {
        FixedWidth.setText(data, CUST_LAST_NAME, v);
    }

    public String custAddrLine1() {
        return FixedWidth.text(data, CUST_ADDR_LINE_1);
    }

    public void setCustAddrLine1(String v) {
        FixedWidth.setText(data, CUST_ADDR_LINE_1, v);
    }

    public String custAddrLine2() {
        return FixedWidth.text(data, CUST_ADDR_LINE_2);
    }

    public void setCustAddrLine2(String v) {
        FixedWidth.setText(data, CUST_ADDR_LINE_2, v);
    }

    public String custAddrLine3() {
        return FixedWidth.text(data, CUST_ADDR_LINE_3);
    }

    public void setCustAddrLine3(String v) {
        FixedWidth.setText(data, CUST_ADDR_LINE_3, v);
    }

    public String custAddrStateCd() {
        return FixedWidth.text(data, CUST_ADDR_STATE_CD);
    }

    public void setCustAddrStateCd(String v) {
        FixedWidth.setText(data, CUST_ADDR_STATE_CD, v);
    }

    public String custAddrCountryCd() {
        return FixedWidth.text(data, CUST_ADDR_COUNTRY_CD);
    }

    public void setCustAddrCountryCd(String v) {
        FixedWidth.setText(data, CUST_ADDR_COUNTRY_CD, v);
    }

    public String custAddrZip() {
        return FixedWidth.text(data, CUST_ADDR_ZIP);
    }

    public void setCustAddrZip(String v) {
        FixedWidth.setText(data, CUST_ADDR_ZIP, v);
    }

    public String custPhoneNum1() {
        return FixedWidth.text(data, CUST_PHONE_NUM_1);
    }

    public void setCustPhoneNum1(String v) {
        FixedWidth.setText(data, CUST_PHONE_NUM_1, v);
    }

    public String custPhoneNum2() {
        return FixedWidth.text(data, CUST_PHONE_NUM_2);
    }

    public void setCustPhoneNum2(String v) {
        FixedWidth.setText(data, CUST_PHONE_NUM_2, v);
    }

    public long custSsn() {
        return FixedWidth.unsigned(data, CUST_SSN);
    }

    public void setCustSsn(long v) {
        FixedWidth.setUnsigned(data, CUST_SSN, v);
    }

    public String custGovtIssuedId() {
        return FixedWidth.text(data, CUST_GOVT_ISSUED_ID);
    }

    public void setCustGovtIssuedId(String v) {
        FixedWidth.setText(data, CUST_GOVT_ISSUED_ID, v);
    }

    public String custDobYyyyMmDd() {
        return FixedWidth.text(data, CUST_DOB_YYYY_MM_DD);
    }

    public void setCustDobYyyyMmDd(String v) {
        FixedWidth.setText(data, CUST_DOB_YYYY_MM_DD, v);
    }

    public String custEftAccountId() {
        return FixedWidth.text(data, CUST_EFT_ACCOUNT_ID);
    }

    public void setCustEftAccountId(String v) {
        FixedWidth.setText(data, CUST_EFT_ACCOUNT_ID, v);
    }

    public String custPriCardHolderInd() {
        return FixedWidth.text(data, CUST_PRI_CARD_HOLDER_IND);
    }

    public void setCustPriCardHolderInd(String v) {
        FixedWidth.setText(data, CUST_PRI_CARD_HOLDER_IND, v);
    }

    public long custFicoCreditScore() {
        return FixedWidth.unsigned(data, CUST_FICO_CREDIT_SCORE);
    }

    public void setCustFicoCreditScore(long v) {
        FixedWidth.setUnsigned(data, CUST_FICO_CREDIT_SCORE, v);
    }

    /** What {@code DISPLAY field} prints for one field (signed numerics get GnuCOBOL's trailing sign). */
    public String display(Field f) {
        return FixedWidth.display(data, f);
    }
}
