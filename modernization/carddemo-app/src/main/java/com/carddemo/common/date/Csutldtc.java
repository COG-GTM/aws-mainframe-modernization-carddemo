package com.carddemo.common.date;

/**
 * {@code CSUTLDTC}: validates a date against a picture mask through {@code CEEDAYS} and returns the 80-byte
 * {@code LS-RESULT} message and {@code RETURN-CODE} (the severity).
 *
 * <p>The message reproduces the baseline byte for byte, including the program's group MOVE of the
 * variable-length string {@code WS-DATE-TO-TEST} into {@code WS-DATE}: its first two bytes are the binary
 * length (X'000A'), so {@code TstDate:} shows NUL, LF and the first 8 characters of the date.
 */
public final class Csutldtc {

    public static final int FIELD_LENGTH = 10;
    public static final int RESULT_LENGTH = 80;

    /**
     * @param feedback   the CEEDAYS condition
     * @param lilian     Lilian day number, 0 unless valid
     * @param message    the 80-character {@code LS-RESULT}
     * @param returnCode {@code RETURN-CODE}, the severity
     */
    public record Result(CeeDays.Feedback feedback, int lilian, String message, int returnCode) {

        public boolean isValid() {
            return feedback == CeeDays.Feedback.VALID;
        }

        /** {@code WS-SEVERITY} as 4 digits. */
        public String severityCode() {
            return message.substring(0, 4);
        }

        /** {@code WS-MSG-NO} as 4 digits. */
        public String messageCode() {
            return message.substring(15, 19);
        }
    }

    private Csutldtc() {
    }

    /** {@code A000-MAIN}: {@code CEEDAYS} on the date and mask, then the severity/message of {@code WS-MESSAGE}. */
    public static Result validate(String date, String mask) {
        String lsDate = fit(date, FIELD_LENGTH);
        String lsMask = fit(mask, FIELD_LENGTH);
        CeeDays.Result days = CeeDays.days(lsDate, lsMask);
        CeeDays.Feedback fb = days.feedback();
        String wsDate = "" + (char) (FIELD_LENGTH >>> 8) + (char) (FIELD_LENGTH & 0xFF) + lsDate.substring(0, 8);
        String message = String.format("%04d", fb.severity())
                + fit("Mesg Code:", 11)
                + String.format("%04d", fb.messageNumber())
                + " "
                + fit(fb.resultText(), 15)
                + " "
                + fit("TstDate:", 9)
                + wsDate
                + " "
                + "Mask used:"
                + lsMask
                + " "
                + "   ";
        return new Result(fb, days.lilian(), message, fb.severity());
    }

    static String fit(String s, int width) {
        return s.length() >= width ? s.substring(0, width) : s + " ".repeat(width - s.length());
    }
}
