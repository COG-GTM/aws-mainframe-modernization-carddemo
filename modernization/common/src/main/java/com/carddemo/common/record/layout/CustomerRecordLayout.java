package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVCUS01Y} CUSTOMER-RECORD: CUSTDATA (VSAM KSDS, key CUST-ID, RECLN 500). */
public final class CustomerRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVCUS01Y", "CUSTOMER-RECORD");

    public static final NumericField CUST_ID = B.numeric("CUST-ID", "9(09)");
    public static final AlphanumericField CUST_FIRST_NAME = B.alpha("CUST-FIRST-NAME", "X(25)");
    public static final AlphanumericField CUST_MIDDLE_NAME = B.alpha("CUST-MIDDLE-NAME", "X(25)");
    public static final AlphanumericField CUST_LAST_NAME = B.alpha("CUST-LAST-NAME", "X(25)");
    public static final AlphanumericField CUST_ADDR_LINE_1 = B.alpha("CUST-ADDR-LINE-1", "X(50)");
    public static final AlphanumericField CUST_ADDR_LINE_2 = B.alpha("CUST-ADDR-LINE-2", "X(50)");
    public static final AlphanumericField CUST_ADDR_LINE_3 = B.alpha("CUST-ADDR-LINE-3", "X(50)");
    public static final AlphanumericField CUST_ADDR_STATE_CD = B.alpha("CUST-ADDR-STATE-CD", "X(02)");
    public static final AlphanumericField CUST_ADDR_COUNTRY_CD = B.alpha("CUST-ADDR-COUNTRY-CD", "X(03)");
    public static final AlphanumericField CUST_ADDR_ZIP = B.alpha("CUST-ADDR-ZIP", "X(10)");
    public static final AlphanumericField CUST_PHONE_NUM_1 = B.alpha("CUST-PHONE-NUM-1", "X(15)");
    public static final AlphanumericField CUST_PHONE_NUM_2 = B.alpha("CUST-PHONE-NUM-2", "X(15)");
    public static final NumericField CUST_SSN = B.numeric("CUST-SSN", "9(09)");
    public static final AlphanumericField CUST_GOVT_ISSUED_ID = B.alpha("CUST-GOVT-ISSUED-ID", "X(20)");
    public static final AlphanumericField CUST_DOB_YYYY_MM_DD = B.alpha("CUST-DOB-YYYY-MM-DD", "X(10)");
    public static final AlphanumericField CUST_EFT_ACCOUNT_ID = B.alpha("CUST-EFT-ACCOUNT-ID", "X(10)");
    public static final AlphanumericField CUST_PRI_CARD_HOLDER_IND = B.alpha("CUST-PRI-CARD-HOLDER-IND", "X(01)");
    public static final NumericField CUST_FICO_CREDIT_SCORE = B.numeric("CUST-FICO-CREDIT-SCORE", "9(03)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(168)");
    public static final RecordLayout LAYOUT = B.build(500);

    private CustomerRecordLayout() {
    }
}
