package com.carddemo.batch.report;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.error.FieldValidationException;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** CORPT00C PROCESS-ENTER-KEY date edits, field by field, in COBOL order. */
class ReportDatesTest {

    private static final LocalDate TODAY = LocalDate.of(2024, 2, 14);

    private static TransactionReportRequest custom(String sm, String sd, String sy, String em, String ed, String ey) {
        return new TransactionReportRequest(ReportType.CUSTOM, sm, sd, sy, em, ed, ey);
    }

    @Test
    void monthlyIsTheCurrentMonthIncludingLeapDay() {
        assertThat(ReportDates.resolve(new TransactionReportRequest(ReportType.MONTHLY, null, null, null, null, null, null), TODAY))
                .isEqualTo(new ReportDates.Range(LocalDate.of(2024, 2, 1), LocalDate.of(2024, 2, 29)));
        assertThat(ReportDates.resolve(new TransactionReportRequest(ReportType.MONTHLY, null, null, null, null, null, null),
                LocalDate.of(2023, 12, 31)))
                .isEqualTo(new ReportDates.Range(LocalDate.of(2023, 12, 1), LocalDate.of(2023, 12, 31)));
    }

    @Test
    void yearlyIsTheCurrentCalendarYear() {
        assertThat(ReportDates.resolve(new TransactionReportRequest(ReportType.YEARLY, "99", null, null, null, null, null), TODAY))
                .isEqualTo(new ReportDates.Range(LocalDate.of(2024, 1, 1), LocalDate.of(2024, 12, 31)));
    }

    @Test
    void noReportTypeAsksForOne() {
        assertError(new TransactionReportRequest(null, null, null, null, null, null, null),
                "reportType", "Select a report type to print report...");
    }

    @ParameterizedTest(name = "{6} -> {7}")
    @CsvSource(delimiter = '|', nullValues = "null", value = {
            "' '|01|2022|07|06|2022|startMonth|Start Date - Month can NOT be empty...",
            "01|null|2022|07|06|2022|startDay|Start Date - Day can NOT be empty...",
            "01|01|''|07|06|2022|startYear|Start Date - Year can NOT be empty...",
            "01|01|2022|''|06|2022|endMonth|End Date - Month can NOT be empty...",
            "01|01|2022|07|' '|2022|endDay|End Date - Day can NOT be empty...",
            "01|01|2022|07|06|null|endYear|End Date - Year can NOT be empty...",
            "13|01|2022|07|06|2022|startMonth|Start Date - Not a valid Month...",
            "01|32|2022|07|06|2022|startDay|Start Date - Not a valid Day...",
            "01|01|2022|99|06|2022|endMonth|End Date - Not a valid Month...",
            "01|01|2022|07|40|2022|endDay|End Date - Not a valid Day...",
            "02|30|2022|07|06|2022|startMonth|Start Date - Not a valid date...",
            "ab|01|2022|07|06|2022|startMonth|Start Date - Not a valid date...",
            "01|01|2022|02|29|2023|endMonth|End Date - Not a valid date...",
            "01|01|2022|04|31|2022|endMonth|End Date - Not a valid date...",
            "01|00|2022|07|06|2022|startMonth|Start Date - Not a valid date..."})
    void customDatesFailWithTheCobolMessage(String sm, String sd, String sy, String em, String ed, String ey,
                                            String field, String message) {
        assertError(custom(sm, sd, sy, em, ed, ey), field, message);
    }

    @Test
    void emptyChecksComeBeforeRangeChecks() {
        // SDTMM is 13 (invalid) but EDTYYYY is empty: the EVALUATE of empty fields runs first.
        assertError(custom("13", "01", "2022", "07", "06", " "), "endYear", "End Date - Year can NOT be empty...");
    }

    @Test
    void numvalCNormalisesTheTypedFields() {
        assertThat(ReportDates.resolve(custom(" 1", "1 ", "2022", "-7", "6", "2022"), TODAY))
                .isEqualTo(new ReportDates.Range(LocalDate.of(2022, 1, 1), LocalDate.of(2022, 7, 6)));
    }

    @Test
    void datesBeforeTheLilianRangeAreAcceptedAsCorpt00cIgnoresMessage2513() {
        assertThat(ReportDates.resolve(custom("01", "01", "1500", "12", "31", "2022"), TODAY).startDate())
                .isEqualTo(LocalDate.of(1500, 1, 1));
        // "99" is year 0099; Feb 30 there is message 2513 too, so it passes.
        assertThat(ReportDates.resolve(custom("02", "30", "99", "12", "31", "2022"), TODAY).startDate())
                .isEqualTo(LocalDate.of(99, 2, 28));
    }

    @Test
    void startAfterEndIsNotAnErrorInCorpt00c() {
        assertThat(ReportDates.resolve(custom("12", "31", "2022", "01", "01", "2022"), TODAY))
                .isEqualTo(new ReportDates.Range(LocalDate.of(2022, 12, 31), LocalDate.of(2022, 1, 1)));
    }

    @Test
    void numvalCOfTextIsZero() {
        assertThat(ReportDates.numval("ab", 100)).isZero();
        assertThat(ReportDates.numval("1a", 100)).isZero();
        assertThat(ReportDates.numval("13", 100)).isEqualTo(13);
        assertThat(ReportDates.numval("99  ", 10_000)).isEqualTo(99);
        assertThat(ReportDates.numval("1.5", 100)).isEqualTo(1);
    }

    private static void assertError(TransactionReportRequest request, String field, String message) {
        assertThatThrownBy(() -> ReportDates.resolve(request, TODAY))
                .isInstanceOf(FieldValidationException.class)
                .hasMessage(message)
                .satisfies(ex -> assertThat(((FieldValidationException) ex).problems().get(0).field()).isEqualTo(field));
    }
}
