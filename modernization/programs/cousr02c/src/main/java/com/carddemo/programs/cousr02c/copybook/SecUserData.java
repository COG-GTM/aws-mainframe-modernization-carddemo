package com.carddemo.programs.cousr02c.copybook;

/**
 * CSUSR01Y - {@code 01 SEC-USER-DATA}: USRSEC VSAM KSDS record, RECORDSIZE(80,80), KEYS(8,0).
 *
 * <pre>
 *   05 SEC-USR-ID      PIC X(08).
 *   05 SEC-USR-FNAME   PIC X(20).
 *   05 SEC-USR-LNAME   PIC X(20).
 *   05 SEC-USR-PWD     PIC X(08).
 *   05 SEC-USR-TYPE    PIC X(01).
 *   05 SEC-USR-FILLER  PIC X(23).
 * </pre>
 *
 * Mutable, like the working-storage area it models; every setter applies MOVE semantics (truncate / space-pad).
 */
public final class SecUserData {

    public static final int RECORD_LENGTH = 80;
    public static final int USR_ID_LEN = 8;
    public static final int USR_FNAME_LEN = 20;
    public static final int USR_LNAME_LEN = 20;
    public static final int USR_PWD_LEN = 8;
    public static final int USR_TYPE_LEN = 1;
    public static final int USR_FILLER_LEN = 23;

    private String secUsrId;
    private String secUsrFname;
    private String secUsrLname;
    private String secUsrPwd;
    private String secUsrType;
    private String secUsrFiller;

    private SecUserData(String record) {
        int p = 0;
        secUsrId = record.substring(p, p += USR_ID_LEN);
        secUsrFname = record.substring(p, p += USR_FNAME_LEN);
        secUsrLname = record.substring(p, p += USR_LNAME_LEN);
        secUsrPwd = record.substring(p, p += USR_PWD_LEN);
        secUsrType = record.substring(p, p += USR_TYPE_LEN);
        secUsrFiller = record.substring(p, p + USR_FILLER_LEN);
    }

    /** Parses an 80-byte record; shorter input is space-padded, longer input truncated (group MOVE). */
    public static SecUserData fromRecord(String record) {
        return new SecUserData(Pic.x(record, RECORD_LENGTH));
    }

    /** Working-storage area with no VALUE clause, modelled as LOW-VALUES. */
    public static SecUserData lowValues() {
        return new SecUserData(Pic.lowValues(RECORD_LENGTH));
    }

    public static SecUserData of(String id, String fname, String lname, String pwd, String type) {
        SecUserData d = fromRecord("");
        d.setSecUsrId(id);
        d.setSecUsrFname(fname);
        d.setSecUsrLname(lname);
        d.setSecUsrPwd(pwd);
        d.setSecUsrType(type);
        return d;
    }

    public String toRecord() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }

    public SecUserData copy() {
        return new SecUserData(toRecord());
    }

    public String getSecUsrId() {
        return secUsrId;
    }

    public void setSecUsrId(String v) {
        secUsrId = Pic.x(v, USR_ID_LEN);
    }

    public String getSecUsrFname() {
        return secUsrFname;
    }

    public void setSecUsrFname(String v) {
        secUsrFname = Pic.x(v, USR_FNAME_LEN);
    }

    public String getSecUsrLname() {
        return secUsrLname;
    }

    public void setSecUsrLname(String v) {
        secUsrLname = Pic.x(v, USR_LNAME_LEN);
    }

    public String getSecUsrPwd() {
        return secUsrPwd;
    }

    public void setSecUsrPwd(String v) {
        secUsrPwd = Pic.x(v, USR_PWD_LEN);
    }

    public String getSecUsrType() {
        return secUsrType;
    }

    public void setSecUsrType(String v) {
        secUsrType = Pic.x(v, USR_TYPE_LEN);
    }

    public String getSecUsrFiller() {
        return secUsrFiller;
    }

    public void setSecUsrFiller(String v) {
        secUsrFiller = Pic.x(v, USR_FILLER_LEN);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof SecUserData other && toRecord().equals(other.toRecord());
    }

    @Override
    public int hashCode() {
        return toRecord().hashCode();
    }

    @Override
    public String toString() {
        return "SecUserData[" + secUsrId + "]";
    }
}
