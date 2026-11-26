package com.carddemo.programs.copaua0c.copybook;

/** WS-IMS-VARIABLES / IMS-RETURN-CODE PIC X(02) and its 88-levels (DIBSTAT values). */
public final class ImsReturnCode {

    public static final String OK = "  ";
    public static final String FW = "FW";
    public static final String SEGMENT_NOT_FOUND = "GE";
    public static final String DUPLICATE_SEGMENT_FOUND = "II";
    public static final String WRONG_PARENTAGE = "GP";
    public static final String END_OF_DB = "GB";
    public static final String DATABASE_UNAVAILABLE = "BA";
    public static final String PSB_SCHEDULED_MORE_THAN_ONCE = "TC";
    public static final String COULD_NOT_SCHEDULE_PSB = "TE";

    private ImsReturnCode() {
    }

    public static boolean isStatusOk(String rc) { return OK.equals(rc) || FW.equals(rc); }
    public static boolean isSegmentNotFound(String rc) { return SEGMENT_NOT_FOUND.equals(rc); }
    public static boolean isDuplicateSegmentFound(String rc) { return DUPLICATE_SEGMENT_FOUND.equals(rc); }
    public static boolean isWrongParentage(String rc) { return WRONG_PARENTAGE.equals(rc); }
    public static boolean isEndOfDb(String rc) { return END_OF_DB.equals(rc); }
    public static boolean isDatabaseUnavailable(String rc) { return DATABASE_UNAVAILABLE.equals(rc); }
    public static boolean isPsbScheduledMoreThanOnce(String rc) { return PSB_SCHEDULED_MORE_THAN_ONCE.equals(rc); }
    public static boolean isCouldNotSchedulePsb(String rc) { return COULD_NOT_SCHEDULE_PSB.equals(rc); }
    public static boolean isRetryCondition(String rc) {
        return "BA".equals(rc) || "FH".equals(rc) || "TE".equals(rc);
    }
}
