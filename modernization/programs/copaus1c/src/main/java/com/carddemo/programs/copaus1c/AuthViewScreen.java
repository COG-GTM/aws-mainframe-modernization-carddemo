package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.Copau1aMap;
import com.carddemo.programs.copaus1c.copybook.Copau1aMap.Field;

import java.util.Map;

/**
 * Response DTO for one EXEC CICS SEND MAP('COPAU1A') MAPSET('COPAU01'): the COPAU1AO output fields at send time
 * (LOW-VALUES, i.e. not transmitted, are kept as NUL characters), the AUTHRSPC colour, the cursor field and ERASE.
 */
public record AuthViewScreen(
        String trnname, String title01, String curdate, String pgmname, String title02, String curtime,
        String cardnum, String authdt, String authtm, String authrsp, String authrsn, String authcd,
        String authamt, String posemd, String authsrc, String mcccd, String crdexp, String authtyp,
        String trnid, String authmtc, String authfrd, String mername, String merid, String mercity,
        String merst, String merzip, String errmsg,
        char authrspColor, Field cursorField, boolean erase) {

    static AuthViewScreen from(Copau1aMap m, boolean erase) {
        Map<Field, String> d = m.snapshot();
        Field cursor = null;
        for (Field f : Field.values()) {
            if (m.getLength(f) == -1) {
                cursor = f;
                break;
            }
        }
        return new AuthViewScreen(
                d.get(Field.TRNNAME), d.get(Field.TITLE01), d.get(Field.CURDATE), d.get(Field.PGMNAME),
                d.get(Field.TITLE02), d.get(Field.CURTIME), d.get(Field.CARDNUM), d.get(Field.AUTHDT),
                d.get(Field.AUTHTM), d.get(Field.AUTHRSP), d.get(Field.AUTHRSN), d.get(Field.AUTHCD),
                d.get(Field.AUTHAMT), d.get(Field.POSEMD), d.get(Field.AUTHSRC), d.get(Field.MCCCD),
                d.get(Field.CRDEXP), d.get(Field.AUTHTYP), d.get(Field.TRNID), d.get(Field.AUTHMTC),
                d.get(Field.AUTHFRD), d.get(Field.MERNAME), d.get(Field.MERID), d.get(Field.MERCITY),
                d.get(Field.MERST), d.get(Field.MERZIP), d.get(Field.ERRMSG),
                m.getColor(Field.AUTHRSP), cursor, erase);
    }
}
