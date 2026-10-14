package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;
import java.math.BigDecimal;

/** CVCUS01Y {@code CUSTOMER-RECORD} (500 bytes): one CUSTDAT KSDS record, key = CUST-ID (offset 0, length 9). */
public final class CustomerRecord extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("CUSTOMER-RECORD");

    /** CUST-ID PIC 9(09). */
    public static final NumericField CUST_ID = B.zoned("CUST-ID", 9, 0, false);

    /** CUST-FIRST-NAME PIC X(25). */
    public static final AlphanumericField FIRST_NAME = B.x("CUST-FIRST-NAME", 25);

    /** CUST-MIDDLE-NAME PIC X(25). */
    public static final AlphanumericField MIDDLE_NAME = B.x("CUST-MIDDLE-NAME", 25);

    /** CUST-LAST-NAME PIC X(25). */
    public static final AlphanumericField LAST_NAME = B.x("CUST-LAST-NAME", 25);

    /** CUST-ADDR-LINE-1 PIC X(50). */
    public static final AlphanumericField ADDR_LINE_1 = B.x("CUST-ADDR-LINE-1", 50);

    /** CUST-ADDR-LINE-2 PIC X(50). */
    public static final AlphanumericField ADDR_LINE_2 = B.x("CUST-ADDR-LINE-2", 50);

    /** CUST-ADDR-LINE-3 PIC X(50). */
    public static final AlphanumericField ADDR_LINE_3 = B.x("CUST-ADDR-LINE-3", 50);

    /** CUST-ADDR-STATE-CD PIC X(2). */
    public static final AlphanumericField ADDR_STATE_CD = B.x("CUST-ADDR-STATE-CD", 2);

    /** CUST-ADDR-COUNTRY-CD PIC X(3). */
    public static final AlphanumericField ADDR_COUNTRY_CD = B.x("CUST-ADDR-COUNTRY-CD", 3);

    /** CUST-ADDR-ZIP PIC X(10). */
    public static final AlphanumericField ADDR_ZIP = B.x("CUST-ADDR-ZIP", 10);

    /** CUST-PHONE-NUM-1 PIC X(15). */
    public static final AlphanumericField PHONE_NUM_1 = B.x("CUST-PHONE-NUM-1", 15);

    /** CUST-PHONE-NUM-2 PIC X(15). */
    public static final AlphanumericField PHONE_NUM_2 = B.x("CUST-PHONE-NUM-2", 15);

    /** CUST-SSN PIC 9(09). */
    public static final NumericField SSN = B.zoned("CUST-SSN", 9, 0, false);

    /** CUST-GOVT-ISSUED-ID PIC X(20). */
    public static final AlphanumericField GOVT_ISSUED_ID = B.x("CUST-GOVT-ISSUED-ID", 20);

    /** CUST-DOB-YYYY-MM-DD PIC X(10). */
    public static final AlphanumericField DOB_YYYY_MM_DD = B.x("CUST-DOB-YYYY-MM-DD", 10);

    /** CUST-EFT-ACCOUNT-ID PIC X(10). */
    public static final AlphanumericField EFT_ACCOUNT_ID = B.x("CUST-EFT-ACCOUNT-ID", 10);

    /** CUST-PRI-CARD-HOLDER-IND PIC X(1). */
    public static final AlphanumericField PRI_CARD_HOLDER_IND = B.x("CUST-PRI-CARD-HOLDER-IND", 1);

    /** CUST-FICO-CREDIT-SCORE PIC 9(03). */
    public static final NumericField FICO_CREDIT_SCORE = B.zoned("CUST-FICO-CREDIT-SCORE", 3, 0, false);

    /** FILLER PIC X(168). */
    public static final RecordLayout LAYOUT = B.filler(168).build();
    public static final int LENGTH = LAYOUT.length();
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 9;

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public CustomerRecord() {
        super(LENGTH);
    }

    public static CustomerRecord of(byte[] bytes) {
        CustomerRecord r = new CustomerRecord();
        r.setBytes(bytes);
        return r;
    }

    public static CustomerRecord of(String record) {
        CustomerRecord r = new CustomerRecord();
        r.move(record);
        return r;
    }

    public BigDecimal custId() {
        return CUST_ID.get(this);
    }

    public String firstName() {
        return FIRST_NAME.get(this);
    }

    public String middleName() {
        return MIDDLE_NAME.get(this);
    }

    public String lastName() {
        return LAST_NAME.get(this);
    }

    public String addrLine1() {
        return ADDR_LINE_1.get(this);
    }

    public String addrLine2() {
        return ADDR_LINE_2.get(this);
    }

    public String addrLine3() {
        return ADDR_LINE_3.get(this);
    }

    public String addrStateCd() {
        return ADDR_STATE_CD.get(this);
    }

    public String addrCountryCd() {
        return ADDR_COUNTRY_CD.get(this);
    }

    public String addrZip() {
        return ADDR_ZIP.get(this);
    }

    public String phoneNum1() {
        return PHONE_NUM_1.get(this);
    }

    public String phoneNum2() {
        return PHONE_NUM_2.get(this);
    }

    public BigDecimal ssn() {
        return SSN.get(this);
    }

    public String govtIssuedId() {
        return GOVT_ISSUED_ID.get(this);
    }

    public String dobYyyyMmDd() {
        return DOB_YYYY_MM_DD.get(this);
    }

    public String eftAccountId() {
        return EFT_ACCOUNT_ID.get(this);
    }

    public String priCardHolderInd() {
        return PRI_CARD_HOLDER_IND.get(this);
    }

    public BigDecimal ficoCreditScore() {
        return FICO_CREDIT_SCORE.get(this);
    }
}
