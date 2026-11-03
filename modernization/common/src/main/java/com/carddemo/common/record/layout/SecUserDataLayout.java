package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CSUSR01Y} SEC-USER-DATA: USRSEC (VSAM KSDS, key SEC-USR-ID, RECLN 80). */
public final class SecUserDataLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CSUSR01Y", "SEC-USER-DATA");

    public static final AlphanumericField SEC_USR_ID = B.alpha("SEC-USR-ID", "X(08)");
    public static final AlphanumericField SEC_USR_FNAME = B.alpha("SEC-USR-FNAME", "X(20)");
    public static final AlphanumericField SEC_USR_LNAME = B.alpha("SEC-USR-LNAME", "X(20)");
    public static final AlphanumericField SEC_USR_PWD = B.alpha("SEC-USR-PWD", "X(08)");
    public static final AlphanumericField SEC_USR_TYPE = B.alpha("SEC-USR-TYPE", "X(01)");
    public static final AlphanumericField SEC_USR_FILLER = B.alpha("SEC-USR-FILLER", "X(23)");
    public static final RecordLayout LAYOUT = B.build(80);

    private SecUserDataLayout() {
    }
}
