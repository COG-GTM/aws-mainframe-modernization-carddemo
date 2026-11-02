package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVTRA05Y} TRAN-RECORD: TRANSACT (VSAM KSDS, key TRAN-ID, AIX on TRAN-PROC-TS, RECLN 350). */
public final class TranRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVTRA05Y", "TRAN-RECORD");

    public static final AlphanumericField TRAN_ID = B.alpha("TRAN-ID", "X(16)");
    public static final AlphanumericField TRAN_TYPE_CD = B.alpha("TRAN-TYPE-CD", "X(02)");
    public static final NumericField TRAN_CAT_CD = B.numeric("TRAN-CAT-CD", "9(04)");
    public static final AlphanumericField TRAN_SOURCE = B.alpha("TRAN-SOURCE", "X(10)");
    public static final AlphanumericField TRAN_DESC = B.alpha("TRAN-DESC", "X(100)");
    public static final NumericField TRAN_AMT = B.numeric("TRAN-AMT", "S9(09)V99");
    public static final NumericField TRAN_MERCHANT_ID = B.numeric("TRAN-MERCHANT-ID", "9(09)");
    public static final AlphanumericField TRAN_MERCHANT_NAME = B.alpha("TRAN-MERCHANT-NAME", "X(50)");
    public static final AlphanumericField TRAN_MERCHANT_CITY = B.alpha("TRAN-MERCHANT-CITY", "X(50)");
    public static final AlphanumericField TRAN_MERCHANT_ZIP = B.alpha("TRAN-MERCHANT-ZIP", "X(10)");
    public static final AlphanumericField TRAN_CARD_NUM = B.alpha("TRAN-CARD-NUM", "X(16)");
    public static final AlphanumericField TRAN_ORIG_TS = B.alpha("TRAN-ORIG-TS", "X(26)");
    public static final AlphanumericField TRAN_PROC_TS = B.alpha("TRAN-PROC-TS", "X(26)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(20)");
    public static final RecordLayout LAYOUT = B.build(350);

    private TranRecordLayout() {
    }
}
