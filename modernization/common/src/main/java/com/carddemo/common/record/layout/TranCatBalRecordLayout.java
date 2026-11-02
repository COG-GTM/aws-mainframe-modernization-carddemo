package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVTRA01Y} TRAN-CAT-BAL-RECORD: TCATBALF (VSAM KSDS, key TRAN-CAT-KEY, RECLN 50). */
public final class TranCatBalRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVTRA01Y", "TRAN-CAT-BAL-RECORD");

    public static final AlphanumericField TRAN_CAT_KEY = B.group("TRAN-CAT-KEY", 17);
    public static final NumericField TRANCAT_ACCT_ID = B.numeric("TRANCAT-ACCT-ID", "9(11)");
    public static final AlphanumericField TRANCAT_TYPE_CD = B.alpha("TRANCAT-TYPE-CD", "X(02)");
    public static final NumericField TRANCAT_CD = B.numeric("TRANCAT-CD", "9(04)");
    public static final NumericField TRAN_CAT_BAL = B.numeric("TRAN-CAT-BAL", "S9(09)V99");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(22)");
    public static final RecordLayout LAYOUT = B.build(50);

    private TranCatBalRecordLayout() {
    }
}
