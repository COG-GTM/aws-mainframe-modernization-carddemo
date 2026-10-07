package com.carddemo.batch.report;

import java.time.LocalDate;

/**
 * A validated report request: the report name and the {@code PARM-START-DATE}/{@code PARM-END-DATE} that CORPT00C
 * substitutes into the TRNRPT00 JCL; {@code start}/{@code end} echo the custom fields as normalised (R-9).
 */
public record ReportWindow(ReportName name, LocalDate startDate, LocalDate endDate, ReportDate start, ReportDate end) {
}
