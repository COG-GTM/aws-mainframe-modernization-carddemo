package com.carddemo.programs.dbunldgs.copybook;

/** Copybook IMSFUNCS (01 FUNC-CODES): DL/I function codes, each PIC X(04). */
public final class ImsFunctionCodes {
    public static final String FUNC_GU = "GU  ";
    public static final String FUNC_GHU = "GHU ";
    public static final String FUNC_GN = "GN  ";
    public static final String FUNC_GHN = "GHN ";
    public static final String FUNC_GNP = "GNP ";
    public static final String FUNC_GHNP = "GHNP";
    public static final String FUNC_REPL = "REPL";
    public static final String FUNC_ISRT = "ISRT";
    public static final String FUNC_DLET = "DLET";
    /** PARMCOUNT PIC S9(05) COMP-5 VALUE +4. */
    public static final int PARMCOUNT = 4;

    private ImsFunctionCodes() {
    }
}
