package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.Field;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

import java.util.ArrayList;
import java.util.List;

/**
 * {@code CVEXPORT} EXPORT-RECORD: the multi-record export file written by {@code CBEXPORT} and read by
 * {@code CBIMPORT} (RECLN 500). {@code EXPORT-REC-TYPE} selects which {@code REDEFINES} view of
 * {@code EXPORT-RECORD-DATA} applies: C customer, A account, X card xref, T transaction, D card
 * ({@code CBEXPORT} lines 274, 343, 407, 462, 527).
 */
public final class ExportRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVEXPORT", "EXPORT-RECORD");

    public static final AlphanumericField EXPORT_REC_TYPE = B.alpha("EXPORT-REC-TYPE", "X(1)");
    public static final AlphanumericField EXPORT_TIMESTAMP = B.alpha("EXPORT-TIMESTAMP", "X(26)");
    public static final NumericField EXPORT_SEQUENCE_NUM = B.numeric("EXPORT-SEQUENCE-NUM", "9(9) COMP");
    public static final AlphanumericField EXPORT_BRANCH_ID = B.alpha("EXPORT-BRANCH-ID", "X(4)");
    public static final AlphanumericField EXPORT_REGION_CODE = B.alpha("EXPORT-REGION-CODE", "X(5)");
    public static final AlphanumericField EXPORT_RECORD_DATA = B.alpha("EXPORT-RECORD-DATA", "X(460)");

    private static final RecordLayout.Builder TS = B.redefines("EXPORT-TIMESTAMP-R", EXPORT_TIMESTAMP);
    public static final AlphanumericField EXPORT_DATE = TS.alpha("EXPORT-DATE", "X(10)");
    public static final AlphanumericField EXPORT_DATE_TIME_SEP = TS.alpha("EXPORT-DATE-TIME-SEP", "X(1)");
    public static final AlphanumericField EXPORT_TIME = TS.alpha("EXPORT-TIME", "X(15)");

    public static final String CUSTOMER_VIEW = "EXPORT-CUSTOMER-DATA";
    private static final RecordLayout.Builder C = B.redefines(CUSTOMER_VIEW, EXPORT_RECORD_DATA);
    public static final NumericField EXP_CUST_ID = C.numeric("EXP-CUST-ID", "9(09) COMP");
    public static final AlphanumericField EXP_CUST_FIRST_NAME = C.alpha("EXP-CUST-FIRST-NAME", "X(25)");
    public static final AlphanumericField EXP_CUST_MIDDLE_NAME = C.alpha("EXP-CUST-MIDDLE-NAME", "X(25)");
    public static final AlphanumericField EXP_CUST_LAST_NAME = C.alpha("EXP-CUST-LAST-NAME", "X(25)");
    public static final List<AlphanumericField> EXP_CUST_ADDR_LINE = C.alphaOccurs("EXP-CUST-ADDR-LINE", "X(50)", 3);
    public static final AlphanumericField EXP_CUST_ADDR_STATE_CD = C.alpha("EXP-CUST-ADDR-STATE-CD", "X(02)");
    public static final AlphanumericField EXP_CUST_ADDR_COUNTRY_CD = C.alpha("EXP-CUST-ADDR-COUNTRY-CD", "X(03)");
    public static final AlphanumericField EXP_CUST_ADDR_ZIP = C.alpha("EXP-CUST-ADDR-ZIP", "X(10)");
    public static final List<AlphanumericField> EXP_CUST_PHONE_NUM = C.alphaOccurs("EXP-CUST-PHONE-NUM", "X(15)", 2);
    public static final NumericField EXP_CUST_SSN = C.numeric("EXP-CUST-SSN", "9(09)");
    public static final AlphanumericField EXP_CUST_GOVT_ISSUED_ID = C.alpha("EXP-CUST-GOVT-ISSUED-ID", "X(20)");
    public static final AlphanumericField EXP_CUST_DOB_YYYY_MM_DD = C.alpha("EXP-CUST-DOB-YYYY-MM-DD", "X(10)");
    public static final AlphanumericField EXP_CUST_EFT_ACCOUNT_ID = C.alpha("EXP-CUST-EFT-ACCOUNT-ID", "X(10)");
    public static final AlphanumericField EXP_CUST_PRI_CARD_HOLDER_IND = C.alpha("EXP-CUST-PRI-CARD-HOLDER-IND", "X(01)");
    public static final NumericField EXP_CUST_FICO_CREDIT_SCORE = C.numeric("EXP-CUST-FICO-CREDIT-SCORE", "9(03) COMP-3");
    public static final AlphanumericField EXP_CUST_FILLER = C.alpha("FILLER", "X(134)");

    public static final String ACCOUNT_VIEW = "EXPORT-ACCOUNT-DATA";
    private static final RecordLayout.Builder A = B.redefines(ACCOUNT_VIEW, EXPORT_RECORD_DATA);
    public static final NumericField EXP_ACCT_ID = A.numeric("EXP-ACCT-ID", "9(11)");
    public static final AlphanumericField EXP_ACCT_ACTIVE_STATUS = A.alpha("EXP-ACCT-ACTIVE-STATUS", "X(01)");
    public static final NumericField EXP_ACCT_CURR_BAL = A.numeric("EXP-ACCT-CURR-BAL", "S9(10)V99 COMP-3");
    public static final NumericField EXP_ACCT_CREDIT_LIMIT = A.numeric("EXP-ACCT-CREDIT-LIMIT", "S9(10)V99");
    public static final NumericField EXP_ACCT_CASH_CREDIT_LIMIT = A.numeric("EXP-ACCT-CASH-CREDIT-LIMIT", "S9(10)V99 COMP-3");
    public static final AlphanumericField EXP_ACCT_OPEN_DATE = A.alpha("EXP-ACCT-OPEN-DATE", "X(10)");
    public static final AlphanumericField EXP_ACCT_EXPIRAION_DATE = A.alpha("EXP-ACCT-EXPIRAION-DATE", "X(10)");
    public static final AlphanumericField EXP_ACCT_REISSUE_DATE = A.alpha("EXP-ACCT-REISSUE-DATE", "X(10)");
    public static final NumericField EXP_ACCT_CURR_CYC_CREDIT = A.numeric("EXP-ACCT-CURR-CYC-CREDIT", "S9(10)V99");
    public static final NumericField EXP_ACCT_CURR_CYC_DEBIT = A.numeric("EXP-ACCT-CURR-CYC-DEBIT", "S9(10)V99 COMP");
    public static final AlphanumericField EXP_ACCT_ADDR_ZIP = A.alpha("EXP-ACCT-ADDR-ZIP", "X(10)");
    public static final AlphanumericField EXP_ACCT_GROUP_ID = A.alpha("EXP-ACCT-GROUP-ID", "X(10)");
    public static final AlphanumericField EXP_ACCT_FILLER = A.alpha("FILLER", "X(352)");

    public static final String TRANSACTION_VIEW = "EXPORT-TRANSACTION-DATA";
    private static final RecordLayout.Builder T = B.redefines(TRANSACTION_VIEW, EXPORT_RECORD_DATA);
    public static final AlphanumericField EXP_TRAN_ID = T.alpha("EXP-TRAN-ID", "X(16)");
    public static final AlphanumericField EXP_TRAN_TYPE_CD = T.alpha("EXP-TRAN-TYPE-CD", "X(02)");
    public static final NumericField EXP_TRAN_CAT_CD = T.numeric("EXP-TRAN-CAT-CD", "9(04)");
    public static final AlphanumericField EXP_TRAN_SOURCE = T.alpha("EXP-TRAN-SOURCE", "X(10)");
    public static final AlphanumericField EXP_TRAN_DESC = T.alpha("EXP-TRAN-DESC", "X(100)");
    public static final NumericField EXP_TRAN_AMT = T.numeric("EXP-TRAN-AMT", "S9(09)V99 COMP-3");
    public static final NumericField EXP_TRAN_MERCHANT_ID = T.numeric("EXP-TRAN-MERCHANT-ID", "9(09) COMP");
    public static final AlphanumericField EXP_TRAN_MERCHANT_NAME = T.alpha("EXP-TRAN-MERCHANT-NAME", "X(50)");
    public static final AlphanumericField EXP_TRAN_MERCHANT_CITY = T.alpha("EXP-TRAN-MERCHANT-CITY", "X(50)");
    public static final AlphanumericField EXP_TRAN_MERCHANT_ZIP = T.alpha("EXP-TRAN-MERCHANT-ZIP", "X(10)");
    public static final AlphanumericField EXP_TRAN_CARD_NUM = T.alpha("EXP-TRAN-CARD-NUM", "X(16)");
    public static final AlphanumericField EXP_TRAN_ORIG_TS = T.alpha("EXP-TRAN-ORIG-TS", "X(26)");
    public static final AlphanumericField EXP_TRAN_PROC_TS = T.alpha("EXP-TRAN-PROC-TS", "X(26)");
    public static final AlphanumericField EXP_TRAN_FILLER = T.alpha("FILLER", "X(140)");

    public static final String CARD_XREF_VIEW = "EXPORT-CARD-XREF-DATA";
    private static final RecordLayout.Builder X = B.redefines(CARD_XREF_VIEW, EXPORT_RECORD_DATA);
    public static final AlphanumericField EXP_XREF_CARD_NUM = X.alpha("EXP-XREF-CARD-NUM", "X(16)");
    public static final NumericField EXP_XREF_CUST_ID = X.numeric("EXP-XREF-CUST-ID", "9(09)");
    public static final NumericField EXP_XREF_ACCT_ID = X.numeric("EXP-XREF-ACCT-ID", "9(11) COMP");
    public static final AlphanumericField EXP_XREF_FILLER = X.alpha("FILLER", "X(427)");

    public static final String CARD_VIEW = "EXPORT-CARD-DATA";
    private static final RecordLayout.Builder D = B.redefines(CARD_VIEW, EXPORT_RECORD_DATA);
    public static final AlphanumericField EXP_CARD_NUM = D.alpha("EXP-CARD-NUM", "X(16)");
    public static final NumericField EXP_CARD_ACCT_ID = D.numeric("EXP-CARD-ACCT-ID", "9(11) COMP");
    public static final NumericField EXP_CARD_CVV_CD = D.numeric("EXP-CARD-CVV-CD", "9(03) COMP");
    public static final AlphanumericField EXP_CARD_EMBOSSED_NAME = D.alpha("EXP-CARD-EMBOSSED-NAME", "X(50)");
    public static final AlphanumericField EXP_CARD_EXPIRAION_DATE = D.alpha("EXP-CARD-EXPIRAION-DATE", "X(10)");
    public static final AlphanumericField EXP_CARD_ACTIVE_STATUS = D.alpha("EXP-CARD-ACTIVE-STATUS", "X(01)");
    public static final AlphanumericField EXP_CARD_FILLER = D.alpha("FILLER", "X(373)");

    public static final RecordLayout LAYOUT = B.build(500);

    private ExportRecordLayout() {
    }

    /** {@code REDEFINES} view of {@code EXPORT-RECORD-DATA} for an {@code EXPORT-REC-TYPE} value. */
    public static String viewFor(String recType) {
        return switch (recType) {
            case "C" -> CUSTOMER_VIEW;
            case "A" -> ACCOUNT_VIEW;
            case "X" -> CARD_XREF_VIEW;
            case "T" -> TRANSACTION_VIEW;
            case "D" -> CARD_VIEW;
            default -> throw new IllegalArgumentException("Unknown EXPORT-REC-TYPE '" + recType + "'");
        };
    }

    /** Storage map of a record of this type: the header fields followed by its data view. */
    public static List<Field> storageFor(String recType) {
        List<Field> storage = new ArrayList<>(LAYOUT.fields().subList(0, 5));
        storage.addAll(LAYOUT.view(viewFor(recType)));
        return List.copyOf(storage);
    }
}
