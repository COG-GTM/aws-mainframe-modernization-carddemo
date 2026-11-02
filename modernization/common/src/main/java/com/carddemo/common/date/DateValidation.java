package com.carddemo.common.date;

import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * {@code CSUTLDTC}: validates a date against a picture string the way the program does through the
 * Language Environment service {@code CEEDAYS}, and returns the same feedback (severity, message
 * number, result text) and the same 80-byte result message. Callers ({@code CORPT00C} 392-426,
 * {@code COTRN02C}) pass the picture {@code YYYY-MM-DD} and test only the severity ({@code '0000'})
 * and, in CORPT00C, message number 2513 (accepted as valid there).
 *
 * <p>The severity / message-number pairs and result texts come from the {@code FC-*} condition
 * tokens and the EVALUATE in CSUTLDTC. Which input yields which condition is CEEDAYS behavior, not
 * visible in the COBOL. <b>Hypothesis</b> (from the IBM LE documentation, not run here: no LE runtime
 * on this VM): the picture tokens {@code YYYY}, {@code MM}, {@code DD} and {@code DDD} with any other
 * non-letter characters as separators; too few characters for the picture is "Insufficient"
 * (2507), a non-digit in a digit position "Nonnumeric data" (2520), month outside 1-12
 * "Invalid month" (2517), a day the month does not have "Datevalue error" (2508), a date before
 * 1582-10-15 (the Lilian day 1) "Unsupp. Range" (2513), any other letter in the picture
 * "Bad Pic String" (2518). Separator characters are not compared with the picture.
 */
public final class DateValidation {

    /** CEEDAYS feedback as CSUTLDTC maps it (the {@code FC-*} 88-levels). */
    public enum Feedback {
        VALID(0, 0, "Date is valid"),
        INSUFFICIENT_DATA(3, 2507, "Insufficient"),
        BAD_DATE_VALUE(3, 2508, "Datevalue error"),
        INVALID_ERA(3, 2509, "Invalid Era"),
        UNSUPPORTED_RANGE(3, 2513, "Unsupp. Range"),
        INVALID_MONTH(3, 2517, "Invalid month"),
        BAD_PIC_STRING(3, 2518, "Bad Pic String"),
        NON_NUMERIC_DATA(3, 2520, "Nonnumeric data"),
        YEAR_IN_ERA_ZERO(3, 2521, "YearInEra is 0");

        private final int severity;
        private final int messageNumber;
        private final String text;

        Feedback(int severity, int messageNumber, String text) {
            this.severity = severity;
            this.messageNumber = messageNumber;
            this.text = text;
        }

        public int severity() {
            return severity;
        }

        public int messageNumber() {
            return messageNumber;
        }

        /** {@code WS-RESULT}. */
        public String text() {
            return text;
        }
    }

    /**
     * One call's outcome. {@code message} is {@code LS-RESULT} (80 bytes); {@link #returnCode()} is the
     * {@code RETURN-CODE} the program sets (the severity).
     */
    public record Result(Feedback feedback, String message) {

        public boolean valid() {
            return feedback == Feedback.VALID;
        }

        /** {@code CSUTLDTC-RESULT-SEV-CD}, e.g. {@code "0000"} or {@code "0003"}. */
        public String severityCode() {
            return message.substring(0, 4);
        }

        /** {@code CSUTLDTC-RESULT-MSG-NUM}, e.g. {@code "2513"}. */
        public String messageNumber() {
            return message.substring(15, 19);
        }

        public int returnCode() {
            return feedback.severity();
        }
    }

    /** Length of {@code LS-DATE} and {@code LS-DATE-FORMAT}. */
    public static final int FIELD_LENGTH = 10;
    public static final int MESSAGE_LENGTH = 80;

    /** Lilian day 1: the first date CEEDAYS accepts. */
    private static final LocalDate LILIAN_START = LocalDate.of(1582, 10, 15);

    private DateValidation() {
    }

    /** {@code CALL 'CSUTLDTC' USING date, format, result}. Both arguments are taken as {@code PIC X(10)}. */
    public static Result validate(String date, String format) {
        String lsDate = fit(date);
        String lsFormat = fit(format);
        Feedback feedback = ceedays(lsDate, lsFormat);
        return new Result(feedback, message(feedback, lsDate, lsFormat));
    }

    /**
     * {@code WS-MESSAGE}: severity, {@code 'Mesg Code:'}, message number, result, {@code 'TstDate:'} with
     * {@code WS-DATE}, {@code 'Mask used:'} with the picture. {@code WS-DATE} is set last by
     * {@code MOVE WS-DATE-TO-TEST TO WS-DATE}: a group move of the variable-length string (2-byte binary
     * length 10, then the text) into {@code X(10)}, so it holds the bytes X'000A' followed by the first 8
     * characters of the date. Kept as is; callers do not read that part.
     */
    static String message(Feedback feedback, String lsDate, String lsFormat) {
        StringBuilder m = new StringBuilder(MESSAGE_LENGTH);
        m.append(String.format(Locale.ROOT, "%04d", feedback.severity()));
        m.append(pad("Mesg Code:", 11));
        m.append(String.format(Locale.ROOT, "%04d", feedback.messageNumber()));
        m.append(' ');
        m.append(pad(feedback.text(), 15));
        m.append(' ');
        m.append(pad("TstDate:", 9));
        m.append('\u0000').append('\n').append(lsDate, 0, 8);
        m.append(' ');
        m.append(pad("Mask used:", 10));
        m.append(lsFormat);
        m.append(' ');
        m.append("   ");
        return m.toString();
    }

    private static Feedback ceedays(String date, String picture) {
        List<String> tokens = tokens(picture.stripTrailing());
        if (tokens == null || tokens.isEmpty()) {
            return Feedback.BAD_PIC_STRING;
        }
        int needed = tokens.stream().mapToInt(String::length).sum();
        if (date.stripTrailing().length() < needed) {
            return Feedback.INSUFFICIENT_DATA;
        }
        Integer year = null;
        Integer month = null;
        Integer day = null;
        Integer dayOfYear = null;
        int pos = 0;
        for (String token : tokens) {
            String part = date.substring(pos, pos + token.length());
            pos += token.length();
            if (!Character.isLetter(token.charAt(0))) {
                continue;
            }
            if (!part.chars().allMatch(c -> c >= '0' && c <= '9')) {
                return Feedback.NON_NUMERIC_DATA;
            }
            int value = Integer.parseInt(part);
            switch (token) {
                case "YYYY" -> year = value;
                case "MM" -> month = value;
                case "DD" -> day = value;
                case "DDD" -> dayOfYear = value;
                default -> {
                    return Feedback.BAD_PIC_STRING;
                }
            }
        }
        if (year == null || (dayOfYear == null && (month == null || day == null))) {
            return Feedback.BAD_PIC_STRING;
        }
        if (month != null && (month < 1 || month > 12)) {
            return Feedback.INVALID_MONTH;
        }
        LocalDate value;
        try {
            value = dayOfYear != null && month == null ? LocalDate.ofYearDay(year, dayOfYear) : LocalDate.of(year, month, day);
        } catch (DateTimeException ex) {
            return year < LILIAN_START.getYear() ? Feedback.UNSUPPORTED_RANGE : Feedback.BAD_DATE_VALUE;
        }
        return value.isBefore(LILIAN_START) ? Feedback.UNSUPPORTED_RANGE : Feedback.VALID;
    }

    /** Splits a picture into YYYY / MM / DD / DDD tokens and one-character separators; null when it has other letters. */
    private static List<String> tokens(String picture) {
        List<String> tokens = new ArrayList<>();
        int i = 0;
        while (i < picture.length()) {
            char c = Character.toUpperCase(picture.charAt(i));
            if (!Character.isLetter(c)) {
                tokens.add(String.valueOf(c));
                i++;
                continue;
            }
            int j = i;
            while (j < picture.length() && Character.toUpperCase(picture.charAt(j)) == c) {
                j++;
            }
            String token = picture.substring(i, j).toUpperCase(Locale.ROOT);
            if (!List.of("YYYY", "MM", "DD", "DDD").contains(token)) {
                return null;
            }
            tokens.add(token);
            i = j;
        }
        return tokens;
    }

    private static String fit(String value) {
        return pad(value == null ? "" : value, FIELD_LENGTH);
    }

    private static String pad(String value, int width) {
        if (value.length() >= width) {
            return value.substring(0, width);
        }
        return value + " ".repeat(width - value.length());
    }
}
