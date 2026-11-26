package com.carddemo.programs.cousr00c.copybook;

import com.carddemo.programs.cousr00c.Cobol;

/** CSUSR01Y - SEC-USER-DATA, the 80-byte USRSEC VSAM KSDS record (key SEC-USR-ID). */
public record SecUserData(
        String secUsrId,      // PIC X(08)
        String secUsrFname,   // PIC X(20)
        String secUsrLname,   // PIC X(20)
        String secUsrPwd,     // PIC X(08)
        String secUsrType,    // PIC X(01)
        String secUsrFiller) { // PIC X(23)

    public static final int RECORD_LENGTH = 80;
    public static final int KEY_LENGTH = 8;

    public SecUserData {
        secUsrId = Cobol.alnum(secUsrId, 8);
        secUsrFname = Cobol.alnum(secUsrFname, 20);
        secUsrLname = Cobol.alnum(secUsrLname, 20);
        secUsrPwd = Cobol.alnum(secUsrPwd, 8);
        secUsrType = Cobol.alnum(secUsrType, 1);
        secUsrFiller = Cobol.alnum(secUsrFiller, 23);
    }

    public static SecUserData of(String id, String fname, String lname, String pwd, String type) {
        return new SecUserData(id, fname, lname, pwd, type, "");
    }

    public static SecUserData fromFixedWidth(String record) {
        String r = Cobol.alnum(record, RECORD_LENGTH);
        return new SecUserData(r.substring(0, 8), r.substring(8, 28), r.substring(28, 48),
                r.substring(48, 56), r.substring(56, 57), r.substring(57, 80));
    }

    public String toFixedWidth() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }

    @Override
    public String toString() {
        return "SecUserData[" + secUsrId + "," + secUsrType + "]";
    }
}
