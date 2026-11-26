package com.carddemo.programs.coadm01c.copybook;

/**
 * CSUSR01Y - 01 SEC-USER-DATA (80 bytes, USRSEC VSAM record). COPY'd by COADM01C but not referenced by its
 * procedure division; translated for completeness.
 */
public record SecUserData(String usrId, String fname, String lname, String pwd, String usrType, String filler) {

    public static final int LENGTH = 80;

    public SecUserData {
        usrId = CobolFields.alnum(usrId, 8);     // SEC-USR-ID     PIC X(08)
        fname = CobolFields.alnum(fname, 20);    // SEC-USR-FNAME  PIC X(20)
        lname = CobolFields.alnum(lname, 20);    // SEC-USR-LNAME  PIC X(20)
        pwd = CobolFields.alnum(pwd, 8);         // SEC-USR-PWD    PIC X(08)
        usrType = CobolFields.alnum(usrType, 1); // SEC-USR-TYPE   PIC X(01)
        filler = CobolFields.alnum(filler, 23);  // SEC-USR-FILLER PIC X(23)
    }

    public static SecUserData fromRecord(String raw) {
        String r = CobolFields.alnum(raw, LENGTH);
        return new SecUserData(r.substring(0, 8), r.substring(8, 28), r.substring(28, 48),
                r.substring(48, 56), r.substring(56, 57), r.substring(57, 80));
    }

    public String toRecord() {
        return usrId + fname + lname + pwd + usrType + filler;
    }
}
