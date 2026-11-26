package com.carddemo.programs.copaus0c;

/** 88-levels of IMS-RETURN-CODE PIC X(02) (DIBSTAT). */
public enum ImsStatus {
    STATUS_OK_BLANK("  "),
    STATUS_OK_FW("FW"),
    SEGMENT_NOT_FOUND("GE"),
    DUPLICATE_SEGMENT_FOUND("II"),
    WRONG_PARENTAGE("GP"),
    END_OF_DB("GB"),
    DATABASE_UNAVAILABLE("BA"),
    PSB_SCHEDULED_MORE_THAN_ONCE("TC"),
    COULD_NOT_SCHEDULE_PSB("TE"),
    OTHER(null);

    public final String code;

    ImsStatus(String code) {
        this.code = code;
    }

    public static ImsStatus of(String dibstat) {
        for (ImsStatus s : values()) {
            if (s.code != null && s.code.equals(dibstat)) {
                return s;
            }
        }
        return OTHER;
    }

    /** 88 STATUS-OK VALUE '  ', 'FW'. */
    public boolean isStatusOk() {
        return this == STATUS_OK_BLANK || this == STATUS_OK_FW;
    }

    /** 88 RETRY-CONDITION VALUE 'BA', 'FH', 'TE'. */
    public static boolean isRetryCondition(String dibstat) {
        return "BA".equals(dibstat) || "FH".equals(dibstat) || "TE".equals(dibstat);
    }
}
