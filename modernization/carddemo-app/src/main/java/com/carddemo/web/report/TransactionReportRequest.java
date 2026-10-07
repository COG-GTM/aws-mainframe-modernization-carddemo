package com.carddemo.web.report;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Size;

/** CORPT0A input: one report type (MONTHLY/YEARLY/CUSTOM selector), the custom dates, and CONFIRM. */
@Schema(description = "Transaction report request (CORPT00C)")
public record TransactionReportRequest(
        @Schema(description = "MONTHLY, YEARLY or CUSTOM (or M/Y/C, any case)", example = "CUSTOM")
        String reportType,
        @Schema(description = "Custom start date (SDTMM/SDTDD/SDTYYYY); ignored for Monthly/Yearly") @Valid
        ReportDateFields startDate,
        @Schema(description = "Custom end date (EDTMM/EDTDD/EDTYYYY); ignored for Monthly/Yearly") @Valid
        ReportDateFields endDate,
        @Schema(description = "CONFIRM: blank = validate and ask, N = clear, Y = submit", example = "Y")
        @Size(max = 1) String confirm) {
}
