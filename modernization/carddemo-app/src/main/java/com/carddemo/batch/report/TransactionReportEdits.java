package com.carddemo.batch.report;

import com.carddemo.common.FieldEditException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.codec.NumvalC;
import com.carddemo.common.date.Csutldtc;
import com.carddemo.common.online.ScreenInput;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Clock;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.List;
import org.springframework.stereotype.Component;

/**
 * CORPT00C (CR00) {@code PROCESS-ENTER-KEY}: builds the report window for the selected report type. Monthly = the
 * current month, Yearly = the current year (from the business clock, {@code FUNCTION CURRENT-DATE}); Custom = the
 * six typed fields, normalised through {@code NUMVAL-C}, range-checked and calendar-checked with CSUTLDTC. The API
 * additionally rejects a start date after the end date (the COBOL does not check it, R-11).
 */
@Component
public class TransactionReportEdits {

    public static final String REPORT_TYPE_FIELD = "reportType";
    public static final String DATE_FORMAT = "YYYY-MM-DD";

    public static final String MSG_SELECT_REPORT = "Select a report type to print report...";
    public static final String MSG_REPORT_TYPE_INVALID = "Report type must be Monthly, Yearly or Custom...";
    public static final String MSG_START_MONTH_EMPTY = "Start Date - Month can NOT be empty...";
    public static final String MSG_START_DAY_EMPTY = "Start Date - Day can NOT be empty...";
    public static final String MSG_START_YEAR_EMPTY = "Start Date - Year can NOT be empty...";
    public static final String MSG_END_MONTH_EMPTY = "End Date - Month can NOT be empty...";
    public static final String MSG_END_DAY_EMPTY = "End Date - Day can NOT be empty...";
    public static final String MSG_END_YEAR_EMPTY = "End Date - Year can NOT be empty...";
    public static final String MSG_START_MONTH_INVALID = "Start Date - Not a valid Month...";
    public static final String MSG_START_DAY_INVALID = "Start Date - Not a valid Day...";
    public static final String MSG_START_YEAR_INVALID = "Start Date - Not a valid Year...";
    public static final String MSG_END_MONTH_INVALID = "End Date - Not a valid Month...";
    public static final String MSG_END_DAY_INVALID = "End Date - Not a valid Day...";
    public static final String MSG_END_YEAR_INVALID = "End Date - Not a valid Year...";
    public static final String MSG_START_DATE_INVALID = "Start Date - Not a valid date...";
    public static final String MSG_END_DATE_INVALID = "End Date - Not a valid date...";
    /** Not in the COBOL (R-11 "No check that start <= end"): an empty window would only print headers. */
    public static final String MSG_START_AFTER_END = "Start Date can NOT be after End Date...";

    private final Clock clock;

    public TransactionReportEdits(Clock clock) {
        this.clock = clock;
    }

    /** R-6, R-7, R-8..R-12, R-13 for the selected {@code reportType}. */
    public ReportWindow edit(String reportType, ReportDate start, ReportDate end) {
        if (ScreenInput.isSpacesOrLowValues(reportType)) {
            throw new InvalidRequestException(REPORT_TYPE_FIELD, MSG_SELECT_REPORT);
        }
        ReportName name = ReportName.of(reportType)
                .orElseThrow(() -> new InvalidRequestException(REPORT_TYPE_FIELD, MSG_REPORT_TYPE_INVALID));
        return switch (name) {
            case MONTHLY -> monthly();
            case YEARLY -> yearly();
            case CUSTOM -> custom(start == null ? ReportDate.EMPTY : start, end == null ? ReportDate.EMPTY : end);
        };
    }

    /** R-6: {@code YYYY-MM-01} .. the day before the first of next month. */
    ReportWindow monthly() {
        LocalDate today = LocalDate.now(clock);
        LocalDate first = today.withDayOfMonth(1);
        LocalDate last = first.plusMonths(1).minusDays(1);
        return new ReportWindow(ReportName.MONTHLY, first, last, echo(first), echo(last));
    }

    /** R-7: {@code YYYY-01-01} .. {@code YYYY-12-31}. */
    ReportWindow yearly() {
        int year = LocalDate.now(clock).getYear();
        LocalDate first = LocalDate.of(year, 1, 1);
        LocalDate last = LocalDate.of(year, 12, 31);
        return new ReportWindow(ReportName.YEARLY, first, last, echo(first), echo(last));
    }

    /** R-8 presence (in screen order), R-9 normalisation, R-10 ranges, R-11 calendar, then start <= end. */
    ReportWindow custom(ReportDate startTyped, ReportDate endTyped) {
        required(startTyped.month(), "startDate.month", MSG_START_MONTH_EMPTY);
        required(startTyped.day(), "startDate.day", MSG_START_DAY_EMPTY);
        required(startTyped.year(), "startDate.year", MSG_START_YEAR_EMPTY);
        required(endTyped.month(), "endDate.month", MSG_END_MONTH_EMPTY);
        required(endTyped.day(), "endDate.day", MSG_END_DAY_EMPTY);
        required(endTyped.year(), "endDate.year", MSG_END_YEAR_EMPTY);
        ReportDate start = normalise(startTyped);
        ReportDate end = normalise(endTyped);
        range(start, "startDate", MSG_START_MONTH_INVALID, MSG_START_DAY_INVALID, MSG_START_YEAR_INVALID);
        range(end, "endDate", MSG_END_MONTH_INVALID, MSG_END_DAY_INVALID, MSG_END_YEAR_INVALID);
        LocalDate startDate = calendar(start, "startDate", MSG_START_DATE_INVALID);
        LocalDate endDate = calendar(end, "endDate", MSG_END_DATE_INVALID);
        if (startDate.isAfter(endDate)) {
            throw new FieldEditException("startDate", MSG_START_AFTER_END, List.of("startDate", "endDate"));
        }
        return new ReportWindow(ReportName.CUSTOM, startDate, endDate, start, end);
    }

    /** R-9: {@code COMPUTE WS-NUM-99 = NUMVAL-C(...)}: invalid text → 0, sign dropped, high-order digits truncated. */
    static ReportDate normalise(ReportDate typed) {
        return new ReportDate(numval(typed.month(), 2), numval(typed.day(), 2), numval(typed.year(), 4));
    }

    static String numval(String typed, int digits) {
        BigInteger value = NumvalC.parse(typed).orElse(BigDecimal.ZERO).abs().toBigInteger()
                .mod(BigInteger.TEN.pow(digits));
        return String.format("%0" + digits + "d", value);
    }

    /** R-10: month {@code > '12'}, day {@code > '31'}, year not numeric (00 passes; CSUTLDTC rejects it). */
    private static void range(ReportDate date, String field, String month, String day, String year) {
        if (!isDigits(date.month()) || date.month().compareTo("12") > 0) {
            throw new FieldEditException(field + ".month", month, List.of(field + ".month"));
        }
        if (!isDigits(date.day()) || date.day().compareTo("31") > 0) {
            throw new FieldEditException(field + ".day", day, List.of(field + ".day"));
        }
        if (!isDigits(date.year())) {
            throw new FieldEditException(field + ".year", year, List.of(field + ".year"));
        }
    }

    /** R-11: valid when severity is 0000 or the message number is 2513 (date outside CEEDAYS' range). */
    private static LocalDate calendar(ReportDate date, String field, String message) {
        String text = date.year() + "-" + date.month() + "-" + date.day();
        Csutldtc.Result result = Csutldtc.validate(text, DATE_FORMAT);
        if (!"0000".equals(result.severityCode()) && !"2513".equals(result.messageCode())) {
            throw new FieldEditException(field, message, List.of(field));
        }
        try {
            return LocalDate.parse(text);
        } catch (DateTimeParseException e) {
            throw new FieldEditException(field, message, List.of(field));
        }
    }

    private static void required(String value, String field, String message) {
        if (ScreenInput.isSpacesOrLowValues(value)) {
            throw new FieldEditException(field, message, List.of(field));
        }
    }

    private static boolean isDigits(String text) {
        return text != null && !text.isEmpty() && text.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    private static ReportDate echo(LocalDate date) {
        return new ReportDate(String.format("%02d", date.getMonthValue()), String.format("%02d", date.getDayOfMonth()),
                String.format("%04d", date.getYear()));
    }
}
