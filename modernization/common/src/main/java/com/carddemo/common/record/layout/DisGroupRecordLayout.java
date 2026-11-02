package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVTRA02Y} DIS-GROUP-RECORD: DISCGRP (VSAM KSDS, key DIS-GROUP-KEY, RECLN 50). */
public final class DisGroupRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVTRA02Y", "DIS-GROUP-RECORD");

    public static final AlphanumericField DIS_GROUP_KEY = B.group("DIS-GROUP-KEY", 16);
    public static final AlphanumericField DIS_ACCT_GROUP_ID = B.alpha("DIS-ACCT-GROUP-ID", "X(10)");
    public static final AlphanumericField DIS_TRAN_TYPE_CD = B.alpha("DIS-TRAN-TYPE-CD", "X(02)");
    public static final NumericField DIS_TRAN_CAT_CD = B.numeric("DIS-TRAN-CAT-CD", "9(04)");
    public static final NumericField DIS_INT_RATE = B.numeric("DIS-INT-RATE", "S9(04)V99");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(28)");
    public static final RecordLayout LAYOUT = B.build(50);

    private DisGroupRecordLayout() {
    }
}
