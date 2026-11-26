package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;

/** Copybook CSUSR01Y: 01 SEC-USER-DATA (80 bytes). COPYed by COTRTLIC but not referenced. */
public record SecUserData(
        /* SEC-USR-ID PIC X(08) */ String usrId,
        /* SEC-USR-FNAME PIC X(20) */ String fname,
        /* SEC-USR-LNAME PIC X(20) */ String lname,
        /* SEC-USR-PWD PIC X(08) */ String pwd,
        /* SEC-USR-TYPE PIC X(01) */ String type,
        /* SEC-USR-FILLER PIC X(23) */ String filler) {

    public static final int LENGTH = 80;

    public static SecUserData fromFixed(String s) {
        String r = Cob.move(s, LENGTH);
        return new SecUserData(r.substring(0, 8), r.substring(8, 28), r.substring(28, 48),
                r.substring(48, 56), r.substring(56, 57), r.substring(57, 80));
    }

    public String toFixed() {
        return Cob.move(usrId, 8) + Cob.move(fname, 20) + Cob.move(lname, 20) + Cob.move(pwd, 8)
                + Cob.move(type, 1) + Cob.move(filler, 23);
    }
}
