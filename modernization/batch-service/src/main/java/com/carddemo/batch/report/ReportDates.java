package com.carddemo.batch.report;

import com.carddemo.common.error.FieldValidationException;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.YearMonth;

/**
 * CORPT00C PROCESS-ENTER-KEY: the date range of a report request, with the COBOL edits in the COBOL
 * order. The first failing edit ends the task (SEND-TRNRPT-SCREEN does {@code GO TO RETURN-TO-CICS}),
 * so exactly one message comes back.
 */
public final class ReportDates {

    public static final String SELECT_REPORT_TYPE = "Select a report type to print report...";

    /** WS-START-DATE / WS-END-DATE: what goes to the SORT INCLUDE and DATEPARM of TRANREPT. */
    public record Range(LocalDate startDate, LocalDate endDate) {
    }

    private ReportDates() {
    }

    public static Range resolve(TransactionReportRequest request, LocalDate today) {
        if (request.reportType() == null) {
            throw new FieldValidationException("reportType", SELECT_REPORT_TYPE);
        }
        return switch (request.reportType()) {
            // FUNCTION CURRENT-DATE: day 01 of this month to the day before the first of next month.
            case MONTHLY -> new Range(today.withDayOfMonth(1), YearMonth.from(today).atEndOfMonth());
            case YEARLY -> new Range(LocalDate.of(today.getYear(), 1, 1), LocalDate.of(today.getYear(), 12, 31));
            case CUSTOM -> custom(request);
        };
    }

    private static Range custom(TransactionReportRequest r) {
        required("startMonth", r.startMonth(), "Start Date - Month can NOT be empty...");
        required("startDay", r.startDay(), "Start Date - Day can NOT be empty...");
        required("startYear", r.startYear(), "Start Date - Year can NOT be empty...");
        required("endMonth", r.endMonth(), "End Date - Month can NOT be empty...");
        required("endDay", r.endDay(), "End Date - Day can NOT be empty...");
        required("endYear", r.endYear(), "End Date - Year can NOT be empty...");

        // COMPUTE WS-NUM-99 / WS-NUM-9999 = FUNCTION NUMVAL-C(field), moved back to the field.
        int startMonth = numval(r.startMonth(), 100);
        int startDay = numval(r.startDay(), 100);
        int startYear = numval(r.startYear(), 10_000);
        int endMonth = numval(r.endMonth(), 100);
        int endDay = numval(r.endDay(), 100);
        int endYear = numval(r.endYear(), 10_000);

        // IS NOT NUMERIC can no longer be true after NUMVAL-C; only the > '12' / > '31' tests bite.
        if (startMonth > 12) {
            throw new FieldValidationException("startMonth", "Start Date - Not a valid Month...");
        }
        if (startDay > 31) {
            throw new FieldValidationException("startDay", "Start Date - Not a valid Day...");
        }
        if (endMonth > 12) {
            throw new FieldValidationException("endMonth", "End Date - Not a valid Month...");
        }
        if (endDay > 31) {
            throw new FieldValidationException("endDay", "End Date - Not a valid Day...");
        }

        LocalDate start = validated(startYear, startMonth, startDay, "startMonth", "Start Date - Not a valid date...");
        LocalDate end = validated(endYear, endMonth, endDay, "endMonth", "End Date - Not a valid date...");
        // CORPT00C does not compare the two dates; a start after the end gives an empty report.
        return new Range(start, end);
    }

    /**
     * {@code CALL 'CSUTLDTC'} with mask {@code YYYY-MM-DD}; anything but severity 0 is an error unless the
     * message number is 2513 (date before 1582-10-15, outside the Lilian range), which CORPT00C lets
     * through and hands to TRANREPT as typed; see {@link #lenient}.
     */
    private static LocalDate validated(int year, int month, int day, String field, String message) {
        Csutldtc feedback = Csutldtc.validate(year, month, day);
        if (feedback == Csutldtc.VALID) {
            return LocalDate.of(year, month, day);
        }
        if (feedback == Csutldtc.UNSUPPORTED_RANGE) {
            return lenient(year, month, day);
        }
        throw new FieldValidationException(field, message);
    }

    /**
     * A 2513 date (year before 1582) accepted by CORPT00C. When it is a real proleptic date it is kept;
     * otherwise (e.g. 0099-02-30) it is clamped to the month's last day, the nearest date TRANREPT's
     * date comparison treats the same for any transaction (all are after 1582).
     */
    static LocalDate lenient(int year, int month, int day) {
        YearMonth ym = YearMonth.of(year, month);
        return ym.atDay(Math.min(Math.max(day, 1), ym.lengthOfMonth()));
    }

    private static void required(String field, String value, String message) {
        if (value == null || value.chars().allMatch(c -> c == ' ' || c == 0)) {
            throw new FieldValidationException(field, message);
        }
    }

    /**
     * NUMVAL-C into an unsigned PIC 99 / PIC 9999: leading/trailing spaces, sign and decimals dropped
     * (the sign is lost, the fraction truncated), high-order digits truncated. Text that is not a number
     * gives 0, as GnuCOBOL does (checked with cobc 3.x; Enterprise COBOL leaves an invalid NUMVAL-C
     * argument undefined, so this is a hypothesis for z/OS).
     */
    static int numval(String text, int modulus) {
        String t = text.strip();
        if (t.startsWith("+") || t.startsWith("-")) {
            t = t.substring(1).strip();
        } else if (t.endsWith("+") || t.endsWith("-")) {
            t = t.substring(0, t.length() - 1).strip();
        }
        if (t.startsWith("$")) {
            t = t.substring(1).strip();
        }
        if (!t.matches("[0-9]*(\\.[0-9]*)?") || t.isEmpty() || t.equals(".")) {
            return 0;
        }
        int dot = t.indexOf('.');
        String whole = dot < 0 ? t : t.substring(0, dot);
        if (whole.isEmpty()) {
            return 0;
        }
        String low = whole.length() > 4 ? whole.substring(whole.length() - 4) : whole;
        return Integer.parseInt(low) % modulus;
    }

    /**
     * The minimal CSUTLDTC (CEEDAYS, mask {@code YYYY-MM-DD}) these edits need, with the feedback codes
     * CORPT00C tests. To be replaced by {@code com.carddemo.common.date.DateValidation} once the
     * CSUTLDTC port from the statements ticket is merged; same semantics.
     */
    enum Csutldtc {
        VALID, INVALID_MONTH, BAD_DATE_VALUE, UNSUPPORTED_RANGE;

        static Csutldtc validate(int year, int month, int day) {
            if (month < 1 || month > 12) {
                return INVALID_MONTH;
            }
            LocalDate value;
            try {
                value = LocalDate.of(year, month, day);
            } catch (DateTimeException ex) {
                return year < 1582 ? UNSUPPORTED_RANGE : BAD_DATE_VALUE;
            }
            return value.isBefore(LocalDate.of(1582, 10, 15)) ? UNSUPPORTED_RANGE : VALID;
        }
    }
}
