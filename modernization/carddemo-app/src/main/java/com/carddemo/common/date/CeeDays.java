package com.carddemo.common.date;

/**
 * {@code CEEDAYS} (LE "convert date to Lilian format") as reproduced by the baseline stub
 * {@code scripts/baseline/stubs/CEEDAYS.cbl}: the picture mask's {@code Y}, {@code M} and {@code D} positions
 * pick the year (up to 4), month and day (up to 2) characters out of the date; other mask characters are
 * separators. Supported years are 1601-9999 (the baseline rejects 1600-01-01 with a bad-date-value token).
 */
public final class CeeDays {

    /** LE feedback-token conditions tested by {@code CSUTLDTC}. */
    public enum Feedback {
        VALID(0, 0, "Date is valid"),
        INSUFFICIENT_DATA(3, 2507, "Insufficient"),
        BAD_DATE_VALUE(3, 2508, "Datevalue error"),
        INVALID_ERA(3, 2509, "Invalid Era"),
        UNSUPPORTED_RANGE(3, 2513, "Unsupp. Range"),
        INVALID_MONTH(3, 2517, "Invalid month"),
        BAD_PICTURE_STRING(3, 2518, "Bad Pic String"),
        NON_NUMERIC_DATA(3, 2520, "Nonnumeric data"),
        YEAR_IN_ERA_ZERO(3, 2521, "YearInEra is 0");

        private final int severity;
        private final int messageNumber;
        private final String resultText;

        Feedback(int severity, int messageNumber, String resultText) {
            this.severity = severity;
            this.messageNumber = messageNumber;
            this.resultText = resultText;
        }

        public int severity() {
            return severity;
        }

        public int messageNumber() {
            return messageNumber;
        }

        /** The {@code WS-RESULT} text {@code CSUTLDTC} moves for this condition. */
        public String resultText() {
            return resultText;
        }

        /** The 8-byte condition token: severity, message number, case/sev control X'59', facility {@code CEE}. */
        public byte[] token() {
            if (this == VALID) {
                return new byte[8];
            }
            return new byte[] {0, (byte) severity, (byte) (messageNumber >>> 8), (byte) messageNumber,
                    0x59, (byte) 0xC3, (byte) 0xC5, (byte) 0xC5};
        }
    }

    /** Lilian day number (0 unless valid) and feedback condition. */
    public record Result(int lilian, Feedback feedback) {
        public boolean isValid() {
            return feedback == Feedback.VALID;
        }
    }

    private CeeDays() {
    }

    public static Result days(String date, String mask) {
        char[] year = "    ".toCharArray();
        char[] month = "  ".toCharArray();
        char[] day = "  ".toCharArray();
        int y = 0;
        int m = 0;
        int d = 0;
        for (int i = 0; i < mask.length(); i++) {
            char c = i < date.length() ? date.charAt(i) : ' ';
            switch (mask.charAt(i)) {
                case 'Y' -> {
                    if (y < 4) {
                        year[y++] = c;
                    }
                }
                case 'M' -> {
                    if (m < 2) {
                        month[m++] = c;
                    }
                }
                case 'D' -> {
                    if (d < 2) {
                        day[d++] = c;
                    }
                }
                default -> {
                    // separator
                }
            }
        }
        String yy = new String(year);
        String mm = new String(month);
        String dd = new String(day);
        if (yy.isBlank() || mm.isBlank() || dd.isBlank()) {
            return new Result(0, Feedback.INSUFFICIENT_DATA);
        }
        if (!digits(yy) || !digits(mm) || !digits(dd)) {
            return new Result(0, Feedback.NON_NUMERIC_DATA);
        }
        int yearN = Integer.parseInt(yy);
        int monthN = Integer.parseInt(mm);
        int dayN = Integer.parseInt(dd);
        if (monthN < 1 || monthN > 12) {
            return new Result(0, Feedback.INVALID_MONTH);
        }
        if (yearN < 1601) {
            return new Result(0, Feedback.BAD_DATE_VALUE);
        }
        if (dayN < 1 || dayN > CobolDates.daysInMonth(yearN, monthN)) {
            return new Result(0, Feedback.BAD_DATE_VALUE);
        }
        int lilian = CobolDates.integerOfDate(yearN * 10000 + monthN * 100 + dayN) + CobolDates.LILIAN_OFFSET;
        return new Result(lilian, Feedback.VALID);
    }

    private static boolean digits(String s) {
        return s.chars().allMatch(c -> c >= '0' && c <= '9');
    }
}
