package com.carddemo.programs.cotrn02c;

import com.carddemo.programs.cotrn02c.copybook.Cotrn2aMap;
import com.carddemo.programs.cotrn02c.copybook.Cotrn2aMap.Field;
import com.carddemo.programs.cotrn02c.copybook.Dfhbmsca;

/**
 * Outbound DTO for {@code SEND MAP('COTRN2A') MAPSET('COTRN02') FROM(COTRN2AO) ERASE CURSOR}: the raw,
 * fixed-width xxxO data of every field, the ERRMSG colour and the field the cursor is placed on.
 */
public record Cotrn2aResponse(
        String trnName, String title01, String curDate, String pgmName, String title02, String curTime,
        String acctId, String cardNum, String typeCd, String catCd, String source, String desc, String amount,
        String origDate, String procDate, String merchantId, String merchantName, String merchantCity,
        String merchantZip, String confirm, String errMsg,
        Dfhbmsca errMsgColor,
        Field cursorField) {

    static Cotrn2aResponse from(Cotrn2aMap m) {
        return new Cotrn2aResponse(
                m.get(Field.TRNNAME), m.get(Field.TITLE01), m.get(Field.CURDATE), m.get(Field.PGMNAME),
                m.get(Field.TITLE02), m.get(Field.CURTIME), m.get(Field.ACTIDIN), m.get(Field.CARDNIN),
                m.get(Field.TTYPCD), m.get(Field.TCATCD), m.get(Field.TRNSRC), m.get(Field.TDESC),
                m.get(Field.TRNAMT), m.get(Field.TORIGDT), m.get(Field.TPROCDT), m.get(Field.MID),
                m.get(Field.MNAME), m.get(Field.MCITY), m.get(Field.MZIP), m.get(Field.CONFIRM),
                m.get(Field.ERRMSG), Dfhbmsca.of(m.f(Field.ERRMSG).getColor()), m.cursorField());
    }

    /** ERRMSG as displayed: trailing spaces / nulls removed. */
    public String message() {
        return rtrim(errMsg);
    }

    static String rtrim(String s) {
        int end = s.length();
        while (end > 0 && (s.charAt(end - 1) == ' ' || s.charAt(end - 1) == '\u0000')) {
            end--;
        }
        return s.substring(0, end);
    }
}
