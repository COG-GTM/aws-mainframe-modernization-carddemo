package com.carddemo.programs.comen01c.copybook;

/**
 * CSUSR01Y - {@code 01 SEC-USER-DATA} (80 bytes, USRSEC file record). COMEN01C copies this layout and
 * names the USRSEC file in WS-USRSEC-FILE but performs no I/O against it.
 */
public record SecUserData(String usrId, String fname, String lname, String pwd, String usrType, String filler) {

    public static final int LENGTH = 80;

    public SecUserData {
        usrId = CobolText.move(usrId, 8);     // SEC-USR-ID     PIC X(08)
        fname = CobolText.move(fname, 20);    // SEC-USR-FNAME  PIC X(20)
        lname = CobolText.move(lname, 20);    // SEC-USR-LNAME  PIC X(20)
        pwd = CobolText.move(pwd, 8);         // SEC-USR-PWD    PIC X(08)
        usrType = CobolText.move(usrType, 1); // SEC-USR-TYPE   PIC X(01)
        filler = CobolText.move(filler, 23);  // SEC-USR-FILLER PIC X(23)
    }

    public static SecUserData parse(String record) {
        String r = CobolText.move(record, LENGTH);
        return new SecUserData(r.substring(0, 8), r.substring(8, 28), r.substring(28, 48),
                r.substring(48, 56), r.substring(56, 57), r.substring(57, 80));
    }

    public String toRecord() {
        return usrId + fname + lname + pwd + usrType + filler;
    }
}
