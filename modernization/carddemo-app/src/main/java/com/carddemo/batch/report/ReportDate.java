package com.carddemo.batch.report;

/** The three CORPT0A fields of a custom date as typed: {@code SDTMM}/{@code SDTDD}/{@code SDTYYYY} (or EDT...). */
public record ReportDate(String month, String day, String year) {

    public static final ReportDate EMPTY = new ReportDate(null, null, null);
}
