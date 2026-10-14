package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.RecordLayout;

/** CSUSR01Y {@code SEC-USER-DATA} (80 bytes): one USRSEC KSDS record, key = SEC-USR-ID (offset 0, length 8). */
public final class UserSecurityRecord extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("SEC-USER-DATA");

    public static final AlphanumericField USR_ID = B.x("SEC-USR-ID", 8);
    public static final AlphanumericField USR_FNAME = B.x("SEC-USR-FNAME", 20);
    public static final AlphanumericField USR_LNAME = B.x("SEC-USR-LNAME", 20);
    public static final AlphanumericField USR_PWD = B.x("SEC-USR-PWD", 8);
    public static final AlphanumericField USR_TYPE = B.x("SEC-USR-TYPE", 1);
    public static final AlphanumericField USR_FILLER = B.x("SEC-USR-FILLER", 23);

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 8;

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public UserSecurityRecord() {
        super(LENGTH);
    }

    public static UserSecurityRecord of(byte[] bytes) {
        UserSecurityRecord r = new UserSecurityRecord();
        r.setBytes(bytes);
        return r;
    }

    public static UserSecurityRecord of(String record) {
        UserSecurityRecord r = new UserSecurityRecord();
        r.move(record);
        return r;
    }

    public String userId() {
        return USR_ID.get(this);
    }

    public String firstName() {
        return USR_FNAME.get(this);
    }

    public String lastName() {
        return USR_LNAME.get(this);
    }

    public String password() {
        return USR_PWD.get(this);
    }

    public String userType() {
        return USR_TYPE.get(this);
    }

    public void setUserId(String v) {
        USR_ID.set(this, v);
    }

    public void setFirstName(String v) {
        USR_FNAME.set(this, v);
    }

    public void setLastName(String v) {
        USR_LNAME.set(this, v);
    }

    public void setPassword(String v) {
        USR_PWD.set(this, v);
    }

    public void setUserType(String v) {
        USR_TYPE.set(this, v);
    }
}
