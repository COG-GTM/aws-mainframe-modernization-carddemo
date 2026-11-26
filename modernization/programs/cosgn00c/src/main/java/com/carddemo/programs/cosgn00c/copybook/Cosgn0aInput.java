package com.carddemo.programs.cosgn00c.copybook;

import static com.carddemo.programs.cosgn00c.copybook.CobolText.lowValues;
import static com.carddemo.programs.cosgn00c.copybook.CobolText.picX;

/**
 * COSGN00 symbolic map, input side (COSGN0AI): the *I data fields after RECEIVE MAP.
 * A {@code null} argument means the field was not transmitted, which BMS delivers as LOW-VALUES;
 * transmitted data is left-justified and space padded to the field length.
 */
public record Cosgn0aInput(
        String trnnameI,   // TRNNAMEI PIC X(4)
        String title01I,   // TITLE01I PIC X(40)
        String curdateI,   // CURDATEI PIC X(8)
        String pgmnameI,   // PGMNAMEI PIC X(8)
        String title02I,   // TITLE02I PIC X(40)
        String curtimeI,   // CURTIMEI PIC X(9)
        String applidI,    // APPLIDI  PIC X(8)
        String sysidI,     // SYSIDI   PIC X(8)
        String useridI,    // USERIDI  PIC X(8)
        String passwdI,    // PASSWDI  PIC X(8)
        String errmsgI) {  // ERRMSGI  PIC X(78)

    public Cosgn0aInput {
        trnnameI = received(trnnameI, Cosgn0aField.TRNNAME);
        title01I = received(title01I, Cosgn0aField.TITLE01);
        curdateI = received(curdateI, Cosgn0aField.CURDATE);
        pgmnameI = received(pgmnameI, Cosgn0aField.PGMNAME);
        title02I = received(title02I, Cosgn0aField.TITLE02);
        curtimeI = received(curtimeI, Cosgn0aField.CURTIME);
        applidI = received(applidI, Cosgn0aField.APPLID);
        sysidI = received(sysidI, Cosgn0aField.SYSID);
        useridI = received(useridI, Cosgn0aField.USERID);
        passwdI = received(passwdI, Cosgn0aField.PASSWD);
        errmsgI = received(errmsgI, Cosgn0aField.ERRMSG);
    }

    /** Only the two unprotected fields (USERID, PASSWD) carry operator input on this screen. */
    public static Cosgn0aInput of(String userId, String password) {
        return new Cosgn0aInput(null, null, null, null, null, null, null, null, userId, password, null);
    }

    /** Masks PASSWDI so the submitted password never appears in logs or diagnostics. */
    @Override
    public String toString() {
        return "Cosgn0aInput[trnnameI=" + trnnameI + ", title01I=" + title01I + ", curdateI=" + curdateI
                + ", pgmnameI=" + pgmnameI + ", title02I=" + title02I + ", curtimeI=" + curtimeI
                + ", applidI=" + applidI + ", sysidI=" + sysidI + ", useridI=" + useridI
                + ", passwdI=********, errmsgI=" + errmsgI + "]";
    }

    private static String received(String value, Cosgn0aField field) {
        return value == null ? lowValues(field.length()) : picX(value, field.length());
    }
}
