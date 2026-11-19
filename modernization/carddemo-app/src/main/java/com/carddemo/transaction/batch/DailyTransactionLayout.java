package com.carddemo.transaction.batch;

import com.carddemo.common.record.RecordLayout;

/** CVTRA06Y DALYTRAN-RECORD (350 bytes, RECFM=FB). */
public final class DailyTransactionLayout {

    public static final int LENGTH = 350;

    public static final RecordLayout LAYOUT = RecordLayout.builder("CVTRA06Y", LENGTH)
            .alphanumeric("DALYTRAN-ID", 16)
            .alphanumeric("DALYTRAN-TYPE-CD", 2)
            .zoned("DALYTRAN-CAT-CD", 4, 0, false)
            .alphanumeric("DALYTRAN-SOURCE", 10)
            .alphanumeric("DALYTRAN-DESC", 100)
            .zoned("DALYTRAN-AMT", 9, 2, true)
            .zoned("DALYTRAN-MERCHANT-ID", 9, 0, false)
            .alphanumeric("DALYTRAN-MERCHANT-NAME", 50)
            .alphanumeric("DALYTRAN-MERCHANT-CITY", 50)
            .alphanumeric("DALYTRAN-MERCHANT-ZIP", 10)
            .alphanumeric("DALYTRAN-CARD-NUM", 16)
            .alphanumeric("DALYTRAN-ORIG-TS", 26)
            .alphanumeric("DALYTRAN-PROC-TS", 26)
            .filler(20)
            .build();

    private DailyTransactionLayout() {
    }
}
