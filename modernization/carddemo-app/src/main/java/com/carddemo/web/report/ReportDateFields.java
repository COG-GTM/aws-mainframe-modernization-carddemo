package com.carddemo.web.report;

import com.carddemo.batch.report.ReportDate;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/** One CORPT0A date: {@code SDTMM}/{@code SDTDD}/{@code SDTYYYY} (or {@code EDT...}) as typed or as normalised. */
@Schema(description = "A custom report date in three screen fields (MM, DD, YYYY)")
public record ReportDateFields(
        @Schema(description = "Month, 2 characters", example = "01") @Size(max = 2) String month,
        @Schema(description = "Day, 2 characters", example = "01") @Size(max = 2) String day,
        @Schema(description = "Year, 4 characters", example = "2022") @Size(max = 4) String year) {

    ReportDate toDomain() {
        return new ReportDate(month, day, year);
    }

    static ReportDateFields of(ReportDate date) {
        return date == null ? null : new ReportDateFields(date.month(), date.day(), date.year());
    }
}
