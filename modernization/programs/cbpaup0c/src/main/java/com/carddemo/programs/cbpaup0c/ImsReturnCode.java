package com.carddemo.programs.cbpaup0c;

import java.util.Set;

/** 88-levels on IMS-RETURN-CODE (WS-IMS-VARIABLES); predicates over a two-character DL/I status code. */
public enum ImsReturnCode {
    STATUS_OK("  ", "FW"),
    SEGMENT_NOT_FOUND("GE"),
    DUPLICATE_SEGMENT_FOUND("II"),
    WRONG_PARENTAGE("GP"),
    END_OF_DB("GB"),
    DATABASE_UNAVAILABLE("BA"),
    PSB_SCHEDULED_MORE_THAN_ONCE("TC"),
    COULD_NOT_SCHEDULE_PSB("TE"),
    RETRY_CONDITION("BA", "FH", "TE");

    /** DIBSTAT after a successful call. */
    public static final String OK = "  ";

    private final Set<String> values;

    ImsReturnCode(String... values) {
        this.values = Set.of(values);
    }

    public boolean matches(String status) {
        return values.contains(status);
    }

    public Set<String> codes() {
        return values;
    }
}
