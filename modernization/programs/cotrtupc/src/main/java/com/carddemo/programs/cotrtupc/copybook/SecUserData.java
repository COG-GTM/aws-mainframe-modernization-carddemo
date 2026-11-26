package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.FixedRecord;

/** CSUSR01Y - SEC-USER-DATA (80 bytes). Copied by COTRTUPC but not referenced in its logic. */
public final class SecUserData extends FixedRecord {

    public static final int LENGTH = 80;

    static final int USR_ID = 0;       // X(08)
    static final int USR_FNAME = 8;    // X(20)
    static final int USR_LNAME = 28;   // X(20)
    static final int USR_PWD = 48;     // X(08)
    static final int USR_TYPE = 56;    // X(01)
    static final int USR_FILLER = 57;  // X(23)

    public SecUserData() {
        super(LENGTH);
    }

    public String secUsrId() { return get(USR_ID, 8); }
    public void secUsrId(String v) { set(USR_ID, 8, v); }
    public String secUsrFname() { return get(USR_FNAME, 20); }
    public void secUsrFname(String v) { set(USR_FNAME, 20, v); }
    public String secUsrLname() { return get(USR_LNAME, 20); }
    public void secUsrLname(String v) { set(USR_LNAME, 20, v); }
    public String secUsrPwd() { return get(USR_PWD, 8); }
    public void secUsrPwd(String v) { set(USR_PWD, 8, v); }
    public String secUsrType() { return get(USR_TYPE, 1); }
    public void secUsrType(String v) { set(USR_TYPE, 1, v); }
    public String secUsrFiller() { return get(USR_FILLER, 23); }
    public void secUsrFiller(String v) { set(USR_FILLER, 23, v); }
}
