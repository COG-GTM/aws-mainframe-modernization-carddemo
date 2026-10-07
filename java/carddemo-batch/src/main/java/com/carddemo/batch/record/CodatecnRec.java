package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

/**
 * Copybook {@code CODATECN} - the parameter block of the assembler date routine {@code COBDATFT}.
 * <pre>
 * 05  CODATECN-IN-REC.
 *     10  CODATECN-TYPE        PIC X.      '1' = YYYYMMDD in, '2' = YYYY-MM-DD in
 *     10  CODATECN-INP-DATE    PIC X(20).
 * 05  CODATECN-OUT-REC.
 *     10  CODATECN-OUTTYPE     PIC X.      '1' = YYYY-MM-DD out, '2' = YYYYMMDD out
 *     10  CODATECN-0UT-DATE    PIC X(20).
 * 05  CODATECN-ERROR-MSG       PIC X(38).
 * </pre>
 * The REDEFINES views of the copybook are character positions inside the two 20-byte dates and are
 * handled positionally by {@link com.carddemo.batch.program.CobDatFt}.
 */
public final class CodatecnRec extends FixedWidthRecord {

    public static final String YYYYMMDD = "1";
    public static final String YYYY_MM_DD = "2";
    public static final String INVALID_INPUT = "INVALID INPUT";

    private static final Layout.Builder B = Layout.builder("CODATECN-REC");
    public static final Field CODATECN_TYPE = B.text("CODATECN-TYPE", 1);
    public static final Field CODATECN_INP_DATE = B.text("CODATECN-INP-DATE", 20);
    public static final Field CODATECN_OUTTYPE = B.text("CODATECN-OUTTYPE", 1);
    public static final Field CODATECN_0UT_DATE = B.text("CODATECN-0UT-DATE", 20);
    public static final Field CODATECN_ERROR_MSG = B.text("CODATECN-ERROR-MSG", 38);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public CodatecnRec() {
        super(LAYOUT);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    public String codatecnType() {
        return FixedWidth.text(data, CODATECN_TYPE);
    }

    public void setCodatecnType(String v) {
        FixedWidth.setText(data, CODATECN_TYPE, v);
    }

    public String codatecnInpDate() {
        return FixedWidth.text(data, CODATECN_INP_DATE);
    }

    public void setCodatecnInpDate(String v) {
        FixedWidth.setText(data, CODATECN_INP_DATE, v);
    }

    public String codatecnOuttype() {
        return FixedWidth.text(data, CODATECN_OUTTYPE);
    }

    public void setCodatecnOuttype(String v) {
        FixedWidth.setText(data, CODATECN_OUTTYPE, v);
    }

    /** The full 20-byte output area; COBDATFT only writes the first 8 or 10 bytes. */
    public String codatecnOutDate() {
        return FixedWidth.text(data, CODATECN_0UT_DATE);
    }

    /** Overwrites only {@code value.length()} bytes starting at 1-based position {@code pos}, like an MVC. */
    public void setCodatecnOutDate(int pos, String value) {
        byte[] src = value.getBytes(FixedWidth.CHARSET);
        System.arraycopy(src, 0, data, CODATECN_0UT_DATE.offset() + pos - 1, src.length);
    }

    public String codatecnErrorMsg() {
        return FixedWidth.text(data, CODATECN_ERROR_MSG);
    }

    public void setCodatecnErrorMsg(String v) {
        FixedWidth.setText(data, CODATECN_ERROR_MSG, v);
    }
}
