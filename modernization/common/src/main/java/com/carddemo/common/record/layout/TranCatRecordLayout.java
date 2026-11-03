package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVTRA04Y} TRAN-CAT-RECORD: TRANCATG (VSAM KSDS, key TRAN-CAT-KEY, RECLN 60). */
public final class TranCatRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVTRA04Y", "TRAN-CAT-RECORD");

    public static final AlphanumericField TRAN_CAT_KEY = B.group("TRAN-CAT-KEY", 6);
    public static final AlphanumericField TRAN_TYPE_CD = B.alpha("TRAN-TYPE-CD", "X(02)");
    public static final NumericField TRAN_CAT_CD = B.numeric("TRAN-CAT-CD", "9(04)");
    public static final AlphanumericField TRAN_CAT_TYPE_DESC = B.alpha("TRAN-CAT-TYPE-DESC", "X(50)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(04)");
    public static final RecordLayout LAYOUT = B.build(60);

    private TranCatRecordLayout() {
    }
}
