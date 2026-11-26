package com.carddemo.programs.copaus1c;

/** IMS-RETURN-CODE PIC X(02) with its 88-level condition names (DIBSTAT values). */
public final class ImsStatus {
    public static final String OK = "  ";
    public static final String SEGMENT_NOT_FOUND = "GE";
    public static final String DUPLICATE_SEGMENT_FOUND = "II";
    public static final String WRONG_PARENTAGE = "GP";
    public static final String END_OF_DB = "GB";
    public static final String DATABASE_UNAVAILABLE = "BA";
    public static final String PSB_SCHEDULED_MORE_THAN_ONCE = "TC";
    public static final String COULD_NOT_SCHEDULE_PSB = "TE";
    /** DIBSTAT returned for a DL/I database call issued while no PSB is scheduled. */
    public static final String PSB_NOT_SCHEDULED = "TH";

    private ImsStatus() {
    }

    public static boolean isStatusOk(String c) { return "  ".equals(c) || "FW".equals(c); }
    public static boolean isSegmentNotFound(String c) { return "GE".equals(c); }
    public static boolean isDuplicateSegmentFound(String c) { return "II".equals(c); }
    public static boolean isWrongParentage(String c) { return "GP".equals(c); }
    public static boolean isEndOfDb(String c) { return "GB".equals(c); }
    public static boolean isDatabaseUnavailable(String c) { return "BA".equals(c); }
    public static boolean isPsbScheduledMoreThanOnce(String c) { return "TC".equals(c); }
    public static boolean isCouldNotSchedulePsb(String c) { return "TE".equals(c); }
    public static boolean isRetryCondition(String c) { return "BA".equals(c) || "FH".equals(c) || "TE".equals(c); }
}
