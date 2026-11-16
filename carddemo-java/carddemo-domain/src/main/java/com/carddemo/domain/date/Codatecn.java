package com.carddemo.domain.date;

/**
 * {@code app/cpy/CODATECN.cpy} ({@code CODATECN-REC}), the record passed to {@code COBDATFT}. Text fields
 * are kept at their {@code PIC X} widths: shorter values are padded with spaces, longer ones cut.
 *
 * @param type      {@code CODATECN-TYPE}: {@code '1'} input is {@code YYYYMMDD}, {@code '2'} input is
 *                  {@code YYYY-MM-DD}
 * @param inpDate   {@code CODATECN-INP-DATE}, {@code PIC X(20)}
 * @param outType   {@code CODATECN-OUTTYPE}: {@code '1'} output {@code YYYY-MM-DD}, {@code '2'} output
 *                  {@code YYYYMMDD}
 * @param outDate   {@code CODATECN-0UT-DATE}, {@code PIC X(20)}
 * @param errorMsg  {@code CODATECN-ERROR-MSG}, {@code PIC X(38)}
 */
public record Codatecn(char type, String inpDate, char outType, String outDate, String errorMsg) {

    public static final char YYYYMMDD_IN = '1';
    public static final char YYYY_MM_DD_IN = '2';
    public static final char YYYY_MM_DD_OP = '1';
    public static final char YYYYMMDD_OP = '2';
    public static final int DATE_LENGTH = 20;
    public static final int ERROR_MSG_LENGTH = 38;

    public Codatecn {
        inpDate = fit(inpDate, DATE_LENGTH);
        outDate = fit(outDate, DATE_LENGTH);
        errorMsg = fit(errorMsg, ERROR_MSG_LENGTH);
    }

    /** A request with blank output fields. */
    public static Codatecn request(char type, String inpDate, char outType) {
        return new Codatecn(type, inpDate, outType, "", "");
    }

    static String fit(String value, int length) {
        String s = value == null ? "" : value;
        return s.length() >= length ? s.substring(0, length) : s + " ".repeat(length - s.length());
    }
}
