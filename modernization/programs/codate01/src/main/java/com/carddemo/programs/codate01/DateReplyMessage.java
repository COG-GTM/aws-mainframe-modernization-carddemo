package com.carddemo.programs.codate01;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/**
 * REPLY-MESSAGE PIC X(1000) as built by 4000-PROCESS-REQUEST-REPLY:
 * {@code 'SYSTEM DATE : ' mm-dd-yyyy 'SYSTEM TIME : ' hh:mm:ss}, space padded.
 */
public record DateReplyMessage(String text) {

    public static final int LENGTH = 1000;
    public static final String DATE_LABEL = "SYSTEM DATE : ";
    public static final String TIME_LABEL = "SYSTEM TIME : ";

    public DateReplyMessage {
        text = alnum(text, LENGTH);
    }

    public static DateReplyMessage of(String mmddyyyy, String time) {
        return new DateReplyMessage(CobolFields.stringInto(alnum("", LENGTH),
                DATE_LABEL, alnum(mmddyyyy, 10), TIME_LABEL, alnum(time, 8)));
    }

    public String systemDate() {
        return text.substring(14, 24);
    }

    public String systemTime() {
        return text.substring(38, 46);
    }
}
