package com.carddemo.programs.dbunldgs;

/** WS-IMS-VARIABLES / IMS-RETURN-CODE PIC X(02) and its 88-level condition names. */
public final class ImsReturnCode {
    private ImsReturnCode() {
    }

    /** 88 STATUS-OK VALUE '  ', 'FW'. */
    public static boolean statusOk(String rc) { return "  ".equals(rc) || "FW".equals(rc); }
    /** 88 SEGMENT-NOT-FOUND VALUE 'GE'. */
    public static boolean segmentNotFound(String rc) { return "GE".equals(rc); }
    /** 88 DUPLICATE-SEGMENT-FOUND VALUE 'II'. */
    public static boolean duplicateSegmentFound(String rc) { return "II".equals(rc); }
    /** 88 WRONG-PARENTAGE VALUE 'GP'. */
    public static boolean wrongParentage(String rc) { return "GP".equals(rc); }
    /** 88 END-OF-DB VALUE 'GB'. */
    public static boolean endOfDb(String rc) { return "GB".equals(rc); }
    /** 88 DATABASE-UNAVAILABLE VALUE 'BA'. */
    public static boolean databaseUnavailable(String rc) { return "BA".equals(rc); }
    /** 88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'. */
    public static boolean psbScheduledMoreThanOnce(String rc) { return "TC".equals(rc); }
    /** 88 COULD-NOT-SCHEDULE-PSB VALUE 'TE'. */
    public static boolean couldNotSchedulePsb(String rc) { return "TE".equals(rc); }
    /** 88 RETRY-CONDITION VALUE 'BA', 'FH', 'TE'. */
    public static boolean retryCondition(String rc) { return "BA".equals(rc) || "FH".equals(rc) || "TE".equals(rc); }
}
