package com.carddemo.common.date;

import java.time.LocalDate;

/**
 * The online date editor of copybook {@code CSUTLDPY} ({@code PERFORM EDIT-DATE-CCYYMMDD THRU
 * EDIT-DATE-CCYYMMDD-EXIT} and {@code EDIT-DATE-OF-BIRTH}) over the {@code CSUTLDWY} work area.
 *
 * <p>The paragraphs fall through: year, month and day are each edited, then day/month/year consistency
 * (31-day months, February 30, February 29 with the copybook's leap rule: years ending {@code 00} divisible
 * by 400, others by 4), then {@code CSUTLDTC} with mask {@code YYYYMMDD}. Only the first message is kept
 * ({@code WS-RETURN-MSG-OFF}). As in the COBOL, reaching {@code EDIT-DATE-LE} always leaves the three flags
 * valid ({@code SET WS-EDIT-DATE-IS-VALID} sits under the {@code EDIT-DATE-LE-EXIT} label), so callers test
 * {@link Result#inputError()}.
 */
public final class CsutldpyDateEdit {

    /** {@code FLG-xxx-ISVALID} (LOW-VALUES), {@code FLG-xxx-NOT-OK} ('0'), {@code FLG-xxx-BLANK} ('B'). */
    public enum Flag { VALID, NOT_OK, BLANK }

    /**
     * @param inputError {@code INPUT-ERROR}
     * @param message    the {@code WS-RETURN-MSG} text, or {@code null} when none was set
     */
    public record Result(boolean inputError, Flag year, Flag month, Flag day, String message) {
    }

    private static final class State {
        final String name;
        boolean inputError;
        Flag year = Flag.NOT_OK;
        Flag month = Flag.NOT_OK;
        Flag day = Flag.NOT_OK;
        String message;

        State(String name) {
            this.name = name.trim();
        }

        void error(String text) {
            inputError = true;
            if (message == null) {
                message = name + text;
            }
        }

        boolean allValid() {
            return year == Flag.VALID && month == Flag.VALID && day == Flag.VALID;
        }

        Result result() {
            return new Result(inputError, year, month, day, message);
        }
    }

    private CsutldpyDateEdit() {
    }

    /**
     * @param variableName {@code WS-EDIT-VARIABLE-NAME}, e.g. {@code Open Date}
     * @param ccyymmdd     {@code WS-EDIT-DATE-CCYYMMDD} (8 characters; shorter input is space padded)
     */
    public static Result editDateCcyymmdd(String variableName, String ccyymmdd) {
        String date = Csutldtc.fit(ccyymmdd, 8);
        State s = new State(variableName);
        String ccyy = date.substring(0, 4);
        String mm = date.substring(4, 6);
        String dd = date.substring(6, 8);

        if (blank(ccyy)) {
            s.year = Flag.BLANK;
            s.error(" : Year must be supplied.");
        } else if (!digits(ccyy)) {
            s.error(" must be 4 digit number.");
        } else if (!ccyy.startsWith("19") && !ccyy.startsWith("20")) {
            s.error(" : Century is not valid.");
        } else {
            s.year = Flag.VALID;
        }

        int month = -1;
        if (blank(mm)) {
            s.month = Flag.BLANK;
            s.error(" : Month must be supplied.");
        } else if (!digits(mm) || Integer.parseInt(mm) < 1 || Integer.parseInt(mm) > 12) {
            s.error(": Month must be a number between 1 and 12.");
        } else {
            month = Integer.parseInt(mm);
            s.month = Flag.VALID;
        }

        int day = -1;
        if (blank(dd)) {
            s.day = Flag.BLANK;
            s.error(" : Day must be supplied.");
        } else {
            Integer value = numval(dd);
            if (value == null || value < 1 || value > 31) {
                s.error(":day must be a number between 1 and 31.");
            } else {
                day = value;
                s.day = Flag.VALID;
            }
        }

        if (month >= 0 && day >= 0) {
            boolean thirtyOneDayMonth = switch (month) {
                case 1, 3, 5, 7, 8, 10, 12 -> true;
                default -> false;
            };
            if (!thirtyOneDayMonth && day == 31) {
                return dayMonthError(s, ":Cannot have 31 days in this month.", false);
            }
            if (month == 2 && day == 30) {
                return dayMonthError(s, ":Cannot have 30 days in this month.", false);
            }
            if (month == 2 && day == 29 && digits(ccyy)) {
                int divisor = ccyy.endsWith("00") ? 400 : 4;
                if (Integer.parseInt(ccyy) % divisor != 0) {
                    return dayMonthError(s, ":Not a leap year.Cannot have 29 days in this month.", true);
                }
            }
        }
        if (!s.allValid()) {
            return s.result();
        }

        String normalised = ccyy + mm + String.format("%02d", day);
        Csutldtc.Result le = Csutldtc.validate(normalised, "YYYYMMDD");
        if (le.returnCode() != 0) {
            s.error(" validation error Sev code: " + le.severityCode() + " Message code: " + le.messageCode());
        }
        s.year = Flag.VALID;
        s.month = Flag.VALID;
        s.day = Flag.VALID;
        return s.result();
    }

    /**
     * {@code EDIT-DATE-OF-BIRTH}: the (already edited, valid) date must be before {@code today}
     * ({@code FUNCTION CURRENT-DATE}).
     */
    public static Result editDateOfBirth(String variableName, String ccyymmdd, LocalDate today) {
        State s = new State(variableName);
        s.year = Flag.VALID;
        s.month = Flag.VALID;
        s.day = Flag.VALID;
        if (!digits(ccyymmdd) || ccyymmdd.length() != 8) {
            throw new IllegalArgumentException("EDIT-DATE-OF-BIRTH needs a numeric CCYYMMDD, got '" + ccyymmdd + "'");
        }
        int birth = CobolDates.integerOfDate(Integer.parseInt(ccyymmdd));
        int current = CobolDates.integerOfDate(today.getYear() * 10000 + today.getMonthValue() * 100
                + today.getDayOfMonth());
        if (current <= birth) {
            s.year = Flag.NOT_OK;
            s.month = Flag.NOT_OK;
            s.day = Flag.NOT_OK;
            s.error(":cannot be in the future ");
        }
        return s.result();
    }

    private static Result dayMonthError(State s, String text, boolean year) {
        s.day = Flag.NOT_OK;
        s.month = Flag.NOT_OK;
        if (year) {
            s.year = Flag.NOT_OK;
        }
        s.error(text);
        return s.result();
    }

    /** {@code FUNCTION TEST-NUMVAL}/{@code NUMVAL} for the two-character day: digits with optional spaces. */
    private static Integer numval(String s) {
        String t = s.strip();
        return digits(t) ? Integer.valueOf(t) : null;
    }

    private static boolean blank(String s) {
        return s.chars().allMatch(c -> c == ' ') || s.chars().allMatch(c -> c == 0);
    }

    private static boolean digits(String s) {
        return !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
    }
}
