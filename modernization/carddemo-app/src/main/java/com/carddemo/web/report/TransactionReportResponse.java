package com.carddemo.web.report;

import com.carddemo.batch.report.ReportStatus;
import com.carddemo.batch.report.TransactionReportService;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDate;

/**
 * CORPT0A after ENTER. {@code VALIDATED}: dates accepted, confirm to submit (R-15); {@code CANCELLED}: screen
 * cleared (R-17); {@code SUBMITTED}: the tranrept stream was queued (R-14/R-18) and {@code executionId} polls it.
 */
@Schema(description = "Transaction report screen after ENTER")
public record TransactionReportResponse(
        ScreenHeader header,
        TransactionReportService.State state,
        @Schema(description = "Monthly, Yearly or Custom", example = "Custom") String reportName,
        @Schema(description = "Start date fields as normalised (R-9)") ReportDateFields startDate,
        @Schema(description = "End date fields as normalised (R-9)") ReportDateFields endDate,
        @Schema(description = "PARM-START-DATE passed to the tranrept stream", example = "2022-01-01")
        LocalDate parmStartDate,
        @Schema(description = "PARM-END-DATE passed to the tranrept stream", example = "2022-07-06")
        LocalDate parmEndDate,
        @Schema(description = "Report execution id (SUBMITTED only)", example = "1") Long executionId,
        @Schema(description = "Execution status at submission (SUBMITTED only)") ReportStatus status,
        @Schema(description = "Where to poll (SUBMITTED only)", example = "/api/v1/reports/transactions/1")
        String statusUrl,
        String message,
        @Schema(description = "PF3: always the main menu (R-4)") NavigationContext exit) {
}
