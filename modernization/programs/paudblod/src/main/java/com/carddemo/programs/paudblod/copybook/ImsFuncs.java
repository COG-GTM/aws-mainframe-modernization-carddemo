package com.carddemo.programs.paudblod.copybook;

/** Copybook IMSFUNCS (01 FUNC-CODES): DL/I function codes, each PIC X(04). */
public enum ImsFuncs {
    FUNC_GU("GU  "),
    FUNC_GHU("GHU "),
    FUNC_GN("GN  "),
    FUNC_GHN("GHN "),
    FUNC_GNP("GNP "),
    FUNC_GHNP("GHNP"),
    FUNC_REPL("REPL"),
    FUNC_ISRT("ISRT"),
    FUNC_DLET("DLET");

    /** 05 PARMCOUNT PIC S9(05) VALUE +4 COMP-5. */
    public static final int PARMCOUNT = 4;

    private final String code;

    ImsFuncs(String code) {
        this.code = code;
    }

    /** The 4-byte function code exactly as passed to CBLTDLI. */
    public String code() {
        return code;
    }
}
