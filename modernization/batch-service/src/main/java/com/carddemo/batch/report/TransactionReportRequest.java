package com.carddemo.batch.report;

import jakarta.validation.constraints.Size;

/**
 * The CORPT0A map input: the report type selected and, for CUSTOM, the six date fields as typed
 * (SDTMM/SDTDD/SDTYYYY, EDTMM/EDTDD/EDTYYYY), kept as text so the CORPT00C edits run on them exactly.
 * CONFIRM is not carried: the POST is the confirmation.
 */
public record TransactionReportRequest(
        ReportType reportType,
        @Size(max = 2) String startMonth,
        @Size(max = 2) String startDay,
        @Size(max = 4) String startYear,
        @Size(max = 2) String endMonth,
        @Size(max = 2) String endDay,
        @Size(max = 4) String endYear) {
}
