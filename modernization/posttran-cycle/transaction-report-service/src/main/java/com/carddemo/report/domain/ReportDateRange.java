package com.carddemo.report.domain;

import com.carddemo.recordio.codec.FixedWidthRecord;

/**
 * CBTRN03C WS-DATEPARM-RECORD (lines 122-125): start X(10), filler X(1), end X(10), read from the
 * first 80-byte record of DATEPARM. Comparison is plain character comparison of yyyy-mm-dd text.
 */
public record ReportDateRange(String startDate, String endDate) {

    public static final int RECORD_LENGTH = 80;

    public static ReportDateRange parse(FixedWidthRecord dateParm) {
        return new ReportDateRange(dateParm.text(0, 10), dateParm.text(11, 10));
    }

    public boolean includes(String date) {
        return date.compareTo(startDate) >= 0 && date.compareTo(endDate) <= 0;
    }
}
