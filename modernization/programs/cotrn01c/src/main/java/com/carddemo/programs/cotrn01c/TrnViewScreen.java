package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Cotrn1a;
import com.carddemo.programs.cotrn01c.copybook.Cotrn1a.Field;

/**
 * Output DTO for one EXEC CICS SEND MAP('COTRN1A') MAPSET('COTRN01') ERASE CURSOR.
 * Values are fixed-width as in COTRN1AO (LOW-VALUES = field not sent / left as default).
 */
public record TrnViewScreen(
        String trnname, String title01, String curdate, String pgmname, String title02, String curtime,
        String trnidin, String trnid, String cardnum, String ttypcd, String tcatcd, String trnsrc,
        String tdesc, String trnamt, String torigdt, String tprocdt, String mid, String mname,
        String mcity, String mzip, String errmsg, Field cursorField) {

    static TrnViewScreen of(Cotrn1a m) {
        return new TrnViewScreen(
                m.get(Field.TRNNAME), m.get(Field.TITLE01), m.get(Field.CURDATE), m.get(Field.PGMNAME),
                m.get(Field.TITLE02), m.get(Field.CURTIME), m.get(Field.TRNIDIN), m.get(Field.TRNID),
                m.get(Field.CARDNUM), m.get(Field.TTYPCD), m.get(Field.TCATCD), m.get(Field.TRNSRC),
                m.get(Field.TDESC), m.get(Field.TRNAMT), m.get(Field.TORIGDT), m.get(Field.TPROCDT),
                m.get(Field.MID), m.get(Field.MNAME), m.get(Field.MCITY), m.get(Field.MZIP),
                m.get(Field.ERRMSG), m.cursorField());
    }
}
