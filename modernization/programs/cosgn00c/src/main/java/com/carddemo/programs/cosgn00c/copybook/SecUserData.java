package com.carddemo.programs.cosgn00c.copybook;

import static com.carddemo.programs.cosgn00c.copybook.CobolText.picX;

/**
 * CSUSR01Y - SEC-USER-DATA, the 80-byte USRSEC VSAM KSDS record (key = SEC-USR-ID, 8 bytes at offset 0).
 * Components are held at their exact PIC width.
 */
public record SecUserData(
        String usrId,     // SEC-USR-ID     PIC X(08)
        String fname,     // SEC-USR-FNAME  PIC X(20)
        String lname,     // SEC-USR-LNAME  PIC X(20)
        String pwd,       // SEC-USR-PWD    PIC X(08)
        String type,      // SEC-USR-TYPE   PIC X(01)
        String filler) {  // SEC-USR-FILLER PIC X(23)

    public static final int LENGTH = 80;
    public static final int KEY_LENGTH = 8;

    public SecUserData {
        usrId = picX(usrId, 8);
        fname = picX(fname, 20);
        lname = picX(lname, 20);
        pwd = picX(pwd, 8);
        type = picX(type, 1);
        filler = picX(filler, 23);
    }

    public static SecUserData of(String usrId, String fname, String lname, String pwd, String type) {
        return new SecUserData(usrId, fname, lname, pwd, type, "");
    }

    /** Parses one fixed-width record; short records are space padded to 80 bytes. */
    public static SecUserData fromRecord(String record) {
        String r = picX(record, LENGTH);
        return new SecUserData(r.substring(0, 8), r.substring(8, 28), r.substring(28, 48),
                r.substring(48, 56), r.substring(56, 57), r.substring(57, 80));
    }

    /** Masks SEC-USR-PWD so stored passwords never appear in logs or diagnostics. */
    @Override
    public String toString() {
        return "SecUserData[usrId=" + usrId + ", fname=" + fname + ", lname=" + lname
                + ", pwd=********, type=" + type + ", filler=" + filler + "]";
    }

    public String toRecord() {
        return usrId + fname + lname + pwd + type + filler;
    }
}
