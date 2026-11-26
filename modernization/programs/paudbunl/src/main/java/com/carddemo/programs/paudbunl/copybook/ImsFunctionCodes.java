package com.carddemo.programs.paudbunl.copybook;

/** IMSFUNCS - 01 FUNC-CODES: DL/I call function codes, each PIC X(04). */
public enum ImsFunctionCodes {
    /** FUNC-GU. */
    GU("GU  "),
    /** FUNC-GHU. */
    GHU("GHU "),
    /** FUNC-GN. */
    GN("GN  "),
    /** FUNC-GHN. */
    GHN("GHN "),
    /** FUNC-GNP. */
    GNP("GNP "),
    /** FUNC-GHNP. */
    GHNP("GHNP"),
    /** FUNC-REPL. */
    REPL("REPL"),
    /** FUNC-ISRT. */
    ISRT("ISRT"),
    /** FUNC-DLET. */
    DLET("DLET");

    /** 05 PARMCOUNT PIC S9(05) VALUE +4 COMP-5. */
    public static final int PARMCOUNT = 4;

    private final String code;

    ImsFunctionCodes(String code) {
        this.code = code;
    }

    /** The 4-byte function code exactly as passed to CBLTDLI. */
    public String code() {
        return code;
    }
}
