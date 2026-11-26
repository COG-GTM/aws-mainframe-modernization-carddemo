package com.carddemo.programs.cousr01c.copybook;

import java.nio.charset.Charset;

/** CSUSR01Y - SEC-USER-DATA, the 80-byte USRSEC VSAM KSDS record (key SEC-USR-ID). */
public record SecUserData(
        String secUsrId,      // PIC X(08)
        String secUsrFname,   // PIC X(20)
        String secUsrLname,   // PIC X(20)
        String secUsrPwd,     // PIC X(08)
        String secUsrType,    // PIC X(01)
        String secUsrFiller   // PIC X(23)
) {

    public static final int LENGTH = 80;
    public static final int KEY_LENGTH = 8;
    public static final Charset EBCDIC = Charset.forName("Cp037");

    public SecUserData {
        secUsrId = Pic.x(secUsrId, 8);
        secUsrFname = Pic.x(secUsrFname, 20);
        secUsrLname = Pic.x(secUsrLname, 20);
        secUsrPwd = Pic.x(secUsrPwd, 8);
        secUsrType = Pic.x(secUsrType, 1);
        secUsrFiller = Pic.x(secUsrFiller, 23);
    }

    public static SecUserData fromFixedWidth(String raw) {
        String s = Pic.x(raw, LENGTH);
        return new SecUserData(s.substring(0, 8), s.substring(8, 28), s.substring(28, 48),
                s.substring(48, 56), s.substring(56, 57), s.substring(57, 80));
    }

    public static SecUserData fromEbcdic(byte[] record) {
        return fromFixedWidth(new String(record, EBCDIC));
    }

    public String toFixedWidth() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }

    public byte[] toEbcdic() {
        return toFixedWidth().getBytes(EBCDIC);
    }
}
