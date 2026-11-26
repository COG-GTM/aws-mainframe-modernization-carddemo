package com.carddemo.programs.coacct01.dto;

import com.carddemo.programs.coacct01.support.CobolText;
import com.carddemo.programs.coacct01.support.EbcdicNumeric;

/**
 * REQUEST-MSG-COPY - the 1000-byte inquiry request read from the input queue.
 *
 * @param func    WS-FUNC   PIC X(04)  ('INQA' = inquire account)
 * @param keyText WS-KEY    PIC 9(11), raw DISPLAY bytes as received
 * @param filler  WS-FILLER PIC X(985)
 */
public record AccountInquiryRequest(String func, String keyText, String filler) {

    public static final int LENGTH = 1000;
    public static final String FUNC_INQUIRE_ACCOUNT = "INQA";

    public AccountInquiryRequest {
        func = CobolText.pic(func, 4);
        keyText = CobolText.pic(keyText, 11);
        filler = CobolText.pic(filler, 985);
    }

    /** INITIALIZE REQUEST-MSG-COPY REPLACING NUMERIC BY ZEROES. */
    public static AccountInquiryRequest initial() {
        return new AccountInquiryRequest("", "0".repeat(11), "");
    }

    /** MOVE REQUEST-MESSAGE TO REQUEST-MSG-COPY (group move of the 1000-byte message). */
    public static AccountInquiryRequest fromMessage(String requestMessage) {
        String m = CobolText.pic(requestMessage, LENGTH);
        return new AccountInquiryRequest(m.substring(0, 4), m.substring(4, 15), m.substring(15));
    }

    /** WS-KEY as a number, evaluated as z/OS does for a numeric comparison (may raise S0C7). */
    public long keyValue() {
        return EbcdicNumeric.value(keyText);
    }

    public String toMessage() {
        return func + keyText + filler;
    }
}
