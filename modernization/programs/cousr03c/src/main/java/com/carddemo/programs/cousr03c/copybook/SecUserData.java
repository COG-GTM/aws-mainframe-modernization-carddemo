package com.carddemo.programs.cousr03c.copybook;

import java.nio.charset.Charset;

/** CSUSR01Y: 01 SEC-USER-DATA, the 80-byte USRSEC VSAM KSDS record (key SEC-USR-ID). */
public class SecUserData extends FixedRecord {

    public static final int LENGTH = 80;
    public static final int KEY_LENGTH = 8;

    static final int USR_ID = 0;       // X(08)
    static final int USR_FNAME = 8;    // X(20)
    static final int USR_LNAME = 28;   // X(20)
    static final int USR_PWD = 48;     // X(08)
    static final int USR_TYPE = 56;    // X(01)
    static final int USR_FILLER = 57;  // X(23)

    /** Values of SEC-USR-TYPE used across CardDemo (no 88-levels in CSUSR01Y itself). */
    public enum UserType {
        ADMIN('A'), USER('U');

        public final char code;

        UserType(char code) {
            this.code = code;
        }
    }

    public SecUserData() {
        super(LENGTH);
    }

    public static SecUserData of(String id, String fname, String lname, String pwd, String type) {
        SecUserData r = new SecUserData();
        r.fill(SPACE);
        r.setUsrId(id);
        r.setUsrFname(fname);
        r.setUsrLname(lname);
        r.setUsrPwd(pwd);
        r.setUsrType(type);
        return r;
    }

    /** Fixed-width decode of one 80-byte record in the given code page (e.g. IBM037 for the EBCDIC sample data). */
    public static SecUserData parse(byte[] record, Charset charset) {
        SecUserData r = new SecUserData();
        r.moveFrom(new String(record, charset).getBytes(java.nio.charset.StandardCharsets.ISO_8859_1));
        return r;
    }

    /** Fixed-width encode of the record in the given code page. */
    public byte[] format(Charset charset) {
        return new String(toBytes(), java.nio.charset.StandardCharsets.ISO_8859_1).getBytes(charset);
    }

    public String usrId() { return getX(USR_ID, 8); }
    public void setUsrId(String v) { setX(USR_ID, 8, v); }
    public String usrFname() { return getX(USR_FNAME, 20); }
    public void setUsrFname(String v) { setX(USR_FNAME, 20, v); }
    public String usrLname() { return getX(USR_LNAME, 20); }
    public void setUsrLname(String v) { setX(USR_LNAME, 20, v); }
    public String usrPwd() { return getX(USR_PWD, 8); }
    public void setUsrPwd(String v) { setX(USR_PWD, 8, v); }
    public String usrType() { return getX(USR_TYPE, 1); }
    public void setUsrType(String v) { setX(USR_TYPE, 1, v); }
    public String usrFiller() { return getX(USR_FILLER, 23); }
    public void setUsrFiller(String v) { setX(USR_FILLER, 23, v); }
    public boolean isAdmin() { return getByte(USR_TYPE) == UserType.ADMIN.code; }
    public boolean isUser() { return getByte(USR_TYPE) == UserType.USER.code; }
}
