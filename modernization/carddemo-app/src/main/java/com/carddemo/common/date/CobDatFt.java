package com.carddemo.common.date;

import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordLayout;

/**
 * {@code COBDATFT} (assembler date formatter) over the {@code CODATECN} record: type {@code 1} turns
 * {@code YYYYMMDD} into {@code YYYY-MM-DD}, type {@code 2} the reverse, by character position only (no
 * calendar check). Type 1 with a {@code -} in position 5 or output type {@code 2}, type 2 with output type
 * {@code 1}, or any other type set {@code CODATECN-ERROR-MSG} to {@code INVALID INPUT}. Only the first 10
 * (type 1) or 8 (type 2) positions of {@code CODATECN-0UT-DATE} are written, as in the baseline stub.
 */
public final class CobDatFt {

    public static final String INVALID_INPUT = "INVALID INPUT";

    private static final RecordLayout CODATECN = Copybook.layout("CODATECN");

    private CobDatFt() {
    }

    public static RecordLayout layout() {
        return CODATECN;
    }

    /** {@code CALL 'COBDATFT' USING CODATECN-REC}; updates {@code rec} in place. */
    public static void call(FixedWidthRecord rec) {
        String type = rec.getString(CODATECN.field("CODATECN-TYPE"));
        String in = rec.getString(CODATECN.field("CODATECN-INP-DATE"));
        String outType = rec.getString(CODATECN.field("CODATECN-OUTTYPE"));
        var outField = CODATECN.field("CODATECN-0UT-DATE");
        StringBuilder out = new StringBuilder(rec.getString(outField));
        switch (type) {
            case "1" -> {
                if (in.charAt(4) == '-' || outType.equals("2")) {
                    error(rec);
                    return;
                }
                out.replace(0, 10, in.substring(0, 4) + "-" + in.substring(4, 6) + "-" + in.substring(6, 8));
            }
            case "2" -> {
                if (outType.equals("1")) {
                    error(rec);
                    return;
                }
                out.replace(0, 8, in.substring(0, 4) + in.substring(5, 7) + in.substring(8, 10));
            }
            default -> {
                error(rec);
                return;
            }
        }
        rec.setString(outField, out.toString());
    }

    /** {@code YYYYMMDD} to {@code YYYY-MM-DD}. */
    public static String toIso(String yyyymmdd) {
        return convert("1", yyyymmdd, "1").substring(0, 10);
    }

    /** {@code YYYY-MM-DD} to {@code YYYYMMDD}. */
    public static String toCompact(String isoDate) {
        return convert("2", isoDate, "2").substring(0, 8);
    }

    private static String convert(String type, String date, String outType) {
        FixedWidthRecord rec = FixedWidthRecord.spaces(CODATECN, com.carddemo.common.codec.RecordEncoding.ASCII);
        rec.setString(CODATECN.field("CODATECN-TYPE"), type);
        rec.setString(CODATECN.field("CODATECN-INP-DATE"), date);
        rec.setString(CODATECN.field("CODATECN-OUTTYPE"), outType);
        call(rec);
        if (!rec.isSpaces(CODATECN.field("CODATECN-ERROR-MSG"))) {
            throw new IllegalArgumentException("COBDATFT " + INVALID_INPUT + ": '" + date + "'");
        }
        return rec.getString(CODATECN.field("CODATECN-0UT-DATE"));
    }

    private static void error(FixedWidthRecord rec) {
        rec.setString(CODATECN.field("CODATECN-ERROR-MSG"), INVALID_INPUT);
    }
}
