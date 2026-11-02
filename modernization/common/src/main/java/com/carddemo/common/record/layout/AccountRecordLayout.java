package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVACT01Y} ACCOUNT-RECORD: ACCTDATA (VSAM KSDS, key ACCT-ID, RECLN 300). */
public final class AccountRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVACT01Y", "ACCOUNT-RECORD");

    public static final NumericField ACCT_ID = B.numeric("ACCT-ID", "9(11)");
    public static final AlphanumericField ACCT_ACTIVE_STATUS = B.alpha("ACCT-ACTIVE-STATUS", "X(01)");
    public static final NumericField ACCT_CURR_BAL = B.numeric("ACCT-CURR-BAL", "S9(10)V99");
    public static final NumericField ACCT_CREDIT_LIMIT = B.numeric("ACCT-CREDIT-LIMIT", "S9(10)V99");
    public static final NumericField ACCT_CASH_CREDIT_LIMIT = B.numeric("ACCT-CASH-CREDIT-LIMIT", "S9(10)V99");
    public static final AlphanumericField ACCT_OPEN_DATE = B.alpha("ACCT-OPEN-DATE", "X(10)");
    public static final AlphanumericField ACCT_EXPIRAION_DATE = B.alpha("ACCT-EXPIRAION-DATE", "X(10)");
    public static final AlphanumericField ACCT_REISSUE_DATE = B.alpha("ACCT-REISSUE-DATE", "X(10)");
    public static final NumericField ACCT_CURR_CYC_CREDIT = B.numeric("ACCT-CURR-CYC-CREDIT", "S9(10)V99");
    public static final NumericField ACCT_CURR_CYC_DEBIT = B.numeric("ACCT-CURR-CYC-DEBIT", "S9(10)V99");
    public static final AlphanumericField ACCT_ADDR_ZIP = B.alpha("ACCT-ADDR-ZIP", "X(10)");
    public static final AlphanumericField ACCT_GROUP_ID = B.alpha("ACCT-GROUP-ID", "X(10)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(178)");
    public static final RecordLayout LAYOUT = B.build(300);

    private AccountRecordLayout() {
    }
}
