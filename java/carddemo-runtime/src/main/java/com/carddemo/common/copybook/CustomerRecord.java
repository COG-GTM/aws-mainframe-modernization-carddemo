package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;
import java.util.List;

/** CVCUS01Y {@code CUSTOMER-RECORD} (RECLN 500): one CUSTDAT KSDS record, key = CUST-ID (offset 0, length 9). */
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
    /** CUST-ADDR-STATE-CD PIC X(02). */
    public static final AlphanumericField ADDR_STATE_CD = B.x("CUST-ADDR-STATE-CD", 2);
    /** CUST-ADDR-COUNTRY-CD PIC X(03). */
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
    /** CUST-PRI-CARD-HOLDER-IND PIC X(01). */
    public static final AlphanumericField PRI_CARD_HOLDER_IND = B.x("CUST-PRI-CARD-HOLDER-IND", 1);
    /** CUST-FICO-CREDIT-SCORE PIC 9(03). */
    public static final NumericField FICO_CREDIT_SCORE = B.zoned("CUST-FICO-CREDIT-SCORE", 3, 0, false);

    static {
        B.filler(168);
    }

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 9;
    /** CSD: DEFINE FILE(CUSTDAT) DSNAME(AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS). */
    public static final String FILE_NAME = "CUSTDAT";
    public static final String DSNAME = "AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS";
    public static final String SEED_RESOURCE = "carddemo/data/ASCII/custdata.txt";

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public CustomerRecord() {
        super(LENGTH);
    }

    public static CustomerRecord of(byte[] bytes) {
        CustomerRecord r = new CustomerRecord();
        r.setBytes(bytes);
        return r;
    }

    /** The 50 customers of app/data/ASCII/custdata.txt. */
    public static List<String> seed() {
        return AsciiSeed.records(SEED_RESOURCE, LENGTH);
    }
}
