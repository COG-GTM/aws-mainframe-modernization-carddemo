package com.carddemo.common.online;

/**
 * {@code WS-FILE-ERROR-MESSAGE} of the online programs ({@code 'File Error: '}, operation, {@code ' on '}, file,
 * RESP/RESP2) as it lands in {@code WS-RETURN-MSG PIC X(75)}. A database failure is reported as
 * {@code DFHRESP(IOERR)} with {@code RESP2} 0.
 */
public final class CicsFileErrors {

    public static final int RESP_IOERR = 17;
    /** {@code WS-RETURN-MSG PIC X(75)}. */
    public static final int RETURN_MSG_LENGTH = 75;

    private CicsFileErrors() {
    }

    public static String message(String operation, String dataset) {
        return returnMessage("File Error: " + fit(operation, 8) + " on " + fit(dataset, 9) + " returned RESP "
                + errorResp(RESP_IOERR) + ",RESP2 " + errorResp(0));
    }

    /** {@code MOVE WS-RESP-CD (S9(9) COMP) TO ERROR-RESP (X(10))}: nine unsigned digits and a space. */
    public static String errorResp(int value) {
        return String.format("%09d ", Math.abs(value));
    }

    /** {@code STRING ... DELIMITED BY SIZE INTO WS-RETURN-MSG}: cut at 75 characters, shown right-trimmed. */
    public static String returnMessage(String text) {
        return ScreenInput.rightTrim(text.length() > RETURN_MSG_LENGTH ? text.substring(0, RETURN_MSG_LENGTH) : text);
    }

    private static String fit(String value, int length) {
        String v = value == null ? "" : value;
        return v.length() >= length ? v.substring(0, length) : v + " ".repeat(length - v.length());
    }
}
