package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVTRA03Y} TRAN-TYPE-RECORD: TRANTYPE (VSAM KSDS, key TRAN-TYPE, RECLN 60). */
public final class TranTypeRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVTRA03Y", "TRAN-TYPE-RECORD");

    public static final AlphanumericField TRAN_TYPE = B.alpha("TRAN-TYPE", "X(02)");
    public static final AlphanumericField TRAN_TYPE_DESC = B.alpha("TRAN-TYPE-DESC", "X(50)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(08)");
    public static final RecordLayout LAYOUT = B.build(60);

    private TranTypeRecordLayout() {
    }
}
