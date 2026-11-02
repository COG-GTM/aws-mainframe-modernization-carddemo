package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVTRA06Y} DALYTRAN-RECORD: DALYTRAN (sequential daily transaction input, RECLN 350). */
public final class DalytranRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVTRA06Y", "DALYTRAN-RECORD");

    public static final AlphanumericField DALYTRAN_ID = B.alpha("DALYTRAN-ID", "X(16)");
    public static final AlphanumericField DALYTRAN_TYPE_CD = B.alpha("DALYTRAN-TYPE-CD", "X(02)");
    public static final NumericField DALYTRAN_CAT_CD = B.numeric("DALYTRAN-CAT-CD", "9(04)");
    public static final AlphanumericField DALYTRAN_SOURCE = B.alpha("DALYTRAN-SOURCE", "X(10)");
    public static final AlphanumericField DALYTRAN_DESC = B.alpha("DALYTRAN-DESC", "X(100)");
    public static final NumericField DALYTRAN_AMT = B.numeric("DALYTRAN-AMT", "S9(09)V99");
    public static final NumericField DALYTRAN_MERCHANT_ID = B.numeric("DALYTRAN-MERCHANT-ID", "9(09)");
    public static final AlphanumericField DALYTRAN_MERCHANT_NAME = B.alpha("DALYTRAN-MERCHANT-NAME", "X(50)");
    public static final AlphanumericField DALYTRAN_MERCHANT_CITY = B.alpha("DALYTRAN-MERCHANT-CITY", "X(50)");
    public static final AlphanumericField DALYTRAN_MERCHANT_ZIP = B.alpha("DALYTRAN-MERCHANT-ZIP", "X(10)");
    public static final AlphanumericField DALYTRAN_CARD_NUM = B.alpha("DALYTRAN-CARD-NUM", "X(16)");
    public static final AlphanumericField DALYTRAN_ORIG_TS = B.alpha("DALYTRAN-ORIG-TS", "X(26)");
    public static final AlphanumericField DALYTRAN_PROC_TS = B.alpha("DALYTRAN-PROC-TS", "X(26)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(20)");
    public static final RecordLayout LAYOUT = B.build(350);

    private DalytranRecordLayout() {
    }
}
