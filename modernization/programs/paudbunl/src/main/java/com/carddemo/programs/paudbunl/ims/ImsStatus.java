package com.carddemo.programs.paudbunl.ims;

/** DL/I status codes, mirroring the 88-levels on WS IMS-RETURN-CODE in PAUDBUNL. */
public final class ImsStatus {
    public static final String OK = "  ";
    public static final String WARNING_FW = "FW";
    public static final String SEGMENT_NOT_FOUND = "GE";
    public static final String DUPLICATE_SEGMENT_FOUND = "II";
    public static final String WRONG_PARENTAGE = "GP";
    public static final String END_OF_DB = "GB";
    public static final String DATABASE_UNAVAILABLE = "BA";
    public static final String PSB_SCHEDULED_MORE_THAN_ONCE = "TC";
    public static final String COULD_NOT_SCHEDULE_PSB = "TE";
    public static final String INVALID_CALL = "AD";

    private ImsStatus() {
    }

    /** 88 STATUS-OK VALUE '  ', 'FW'. */
    public static boolean isStatusOk(String s) {
        return OK.equals(s) || WARNING_FW.equals(s);
    }

    /** 88 RETRY-CONDITION VALUE 'BA', 'FH', 'TE'. */
    public static boolean isRetryCondition(String s) {
        return "BA".equals(s) || "FH".equals(s) || "TE".equals(s);
    }
}
