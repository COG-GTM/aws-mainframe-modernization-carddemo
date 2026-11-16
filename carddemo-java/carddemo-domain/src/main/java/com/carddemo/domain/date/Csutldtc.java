package com.carddemo.domain.date;

import java.nio.ByteBuffer;
import java.util.Locale;

/**
 * {@code app/cbl/CSUTLDTC.cbl}: validates a date against a date format through {@code CEEDAYS} and
 * returns the 80-byte {@code LS-RESULT} message and {@code RETURN-CODE} the program sets.
 *
 * <p>Callers ({@code CORPT00C}, {@code COTRN02C}, the {@code CSUTLDPY} copybook) pass {@code PIC X(10)}
 * fields with the format {@code YYYY-MM-DD} or {@code YYYYMMDD}, treat severity {@code 0000} as valid,
 * and {@code CORPT00C}/{@code COTRN02C} also accept message number {@code 2513}.
 *
 * <p>{@code LS-RESULT} is {@code WS-MESSAGE}: severity (4), {@code 'Mesg Code:'} (11), message number
 * (4), space, {@code WS-RESULT} (15), space, {@code 'TstDate:'} (9), {@code WS-DATE} (10), space,
 * {@code 'Mask used:'} (10), {@code WS-DATE-FMT} (10), space, 3 spaces. {@code WS-DATE} comes from
 * {@code MOVE WS-DATE-TO-TEST TO WS-DATE}, a group move of the variable-length string, so it holds the
 * 2-byte binary length X'000A' and then the first 8 characters of the date; here those two bytes are
 * {@code U+0000 U+000A}. Callers never read that part, but it is kept so the message matches the
 * program byte for byte (checked against GnuCOBOL in {@code CsutldtcGnuCobolTest}).
 */
public final class Csutldtc {

    /** Length of {@code LS-DATE} and {@code LS-DATE-FORMAT}. */
    public static final int FIELD_LENGTH = 10;
    /** Length of {@code LS-RESULT}. */
    public static final int RESULT_LENGTH = 80;
    /** Severity code callers treat as a valid date. */
    public static final String VALID_SEVERITY = "0000";

    /** {@code LS-RESULT} and {@code RETURN-CODE}, with the fields callers read through {@code CSUTLDTC-RESULT}. */
    public record Result(String message, int returnCode) {

        /** {@code CSUTLDTC-RESULT-SEV-CD}, e.g. {@code "0000"}. */
        public String severityCode() {
            return message.substring(0, 4);
        }

        /** {@code CSUTLDTC-RESULT-MSG-NUM}, e.g. {@code "2513"}. */
        public String messageNumber() {
            return message.substring(15, 19);
        }

        /** {@code WS-RESULT}, e.g. {@code "Datevalue error"}. */
        public String resultText() {
            return message.substring(20, 35);
        }

        /** The test every caller makes: {@code CSUTLDTC-RESULT-SEV-CD = '0000'}. */
        public boolean valid() {
            return VALID_SEVERITY.equals(severityCode());
        }
    }

    private Csutldtc() {
    }

    /** {@code CALL 'CSUTLDTC' USING date, date-format, result}; both inputs are read as {@code PIC X(10)}. */
    public static Result call(String date, String dateFormat) {
        String lsDate = fit(date);
        String lsDateFormat = fit(dateFormat);
        Ceedays.Result ceedays = Ceedays.ceedays(lsDate, lsDateFormat);
        return a000Main(ceedays.feedback().token(), lsDate, lsDateFormat);
    }

    /** {@code A000-MAIN} after the CEEDAYS call, for any 8-byte feedback token. */
    static Result a000Main(byte[] token, String lsDate, String lsDateFormat) {
        ByteBuffer fc = ByteBuffer.wrap(token);
        int severity = Short.toUnsignedInt(fc.getShort(0));
        int messageNumber = Short.toUnsignedInt(fc.getShort(2));
        CeedaysFeedback feedback = CeedaysFeedback.ofToken(token);
        String result = feedback == null ? CeedaysFeedback.OTHER_RESULT : feedback.result();

        StringBuilder m = new StringBuilder(RESULT_LENGTH);
        m.append(String.format(Locale.ROOT, "%04d", severity % 10000));
        m.append("Mesg Code: ");
        m.append(String.format(Locale.ROOT, "%04d", messageNumber % 10000));
        m.append(' ');
        m.append(result);
        m.append(' ');
        m.append("TstDate: ");
        m.append('\u0000').append((char) FIELD_LENGTH).append(lsDate, 0, FIELD_LENGTH - 2);
        m.append(' ');
        m.append("Mask used:");
        m.append(lsDateFormat);
        m.append(' ');
        m.append("   ");
        return new Result(m.toString(), severity % 10000);
    }

    private static String fit(String value) {
        String s = value == null ? "" : value;
        return s.length() >= FIELD_LENGTH ? s.substring(0, FIELD_LENGTH) : s + " ".repeat(FIELD_LENGTH - s.length());
    }
}
