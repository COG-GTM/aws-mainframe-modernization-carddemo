package com.carddemo.programs.codate01;

import static com.carddemo.programs.codate01.CobolFields.alnum;

/**
 * WORKING-STORAGE 01 REQUEST-MSG-COPY (1000 bytes): the inbound date-request message.
 * CODATE01 populates it but never inspects it - any request yields the system date reply.
 *
 * @param func   WS-FUNC PIC X(04)
 * @param key    WS-KEY PIC 9(11), kept as the raw 11 bytes because it is filled by a group MOVE
 * @param filler WS-FILLER PIC X(985)
 */
public record RequestMsgCopy(String func, String key, String filler) {

    public static final int LENGTH = 1000;

    public RequestMsgCopy {
        func = alnum(func, 4);
        key = alnum(key, 11);
        filler = alnum(filler, 985);
    }

    /** VALUE clauses: SPACES / ZEROES / SPACES. */
    public static RequestMsgCopy initial() {
        return new RequestMsgCopy("", "00000000000", "");
    }

    /** MOVE REQUEST-MESSAGE TO REQUEST-MSG-COPY (group move of the 1000-byte buffer). */
    public static RequestMsgCopy fromMessage(String requestMessage) {
        String m = alnum(requestMessage, LENGTH);
        return new RequestMsgCopy(m.substring(0, 4), m.substring(4, 15), m.substring(15));
    }

    /** INITIALIZE REQUEST-MSG-COPY REPLACING NUMERIC BY ZEROES: only the numeric WS-KEY changes. */
    public RequestMsgCopy initializeNumericToZeroes() {
        return new RequestMsgCopy(func, "00000000000", filler);
    }

    public boolean keyIsNumeric() {
        return key.chars().allMatch(c -> c >= '0' && c <= '9');
    }

    public String render() {
        return func + key + filler;
    }
}
