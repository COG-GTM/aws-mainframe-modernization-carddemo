package com.carddemo.programs.cousr03c.dto;

import com.carddemo.programs.cousr03c.copybook.Cousr03Map;

/**
 * Snapshot of COUSR3AO as sent by EXEC CICS SEND MAP('COUSR3A') ERASE CURSOR. Values are the raw
 * fixed-width O fields (LOW-VALUES appear as U+0000). {@code errMsgColor} is the ERRMSGC name.
 */
public record UserDeleteScreen(
        String trnName,
        String title01,
        String curDate,
        String pgmName,
        String title02,
        String curTime,
        String usrIdIn,
        String fname,
        String lname,
        String usrType,
        String errMsg,
        String errMsgColor,
        Cousr03Map.Field cursorField) {

    /** Displayed text of a field: trailing spaces and LOW-VALUES removed. */
    public static String text(String field) {
        int end = field.length();
        while (end > 0 && (field.charAt(end - 1) == ' ' || field.charAt(end - 1) == 0)) {
            end--;
        }
        return field.substring(0, end);
    }
}
