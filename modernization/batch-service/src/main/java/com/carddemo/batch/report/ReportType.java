package com.carddemo.batch.report;

/** CORPT00C's three selection fields: MONTHLY, YEARLY, CUSTOM (WS-REPORT-NAME Monthly / Yearly / Custom). */
public enum ReportType {
    MONTHLY("Monthly"),
    YEARLY("Yearly"),
    CUSTOM("Custom");

    private final String reportName;

    ReportType(String reportName) {
        this.reportName = reportName;
    }

    /** {@code WS-REPORT-NAME}. */
    public String reportName() {
        return reportName;
    }
}
