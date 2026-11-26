package com.carddemo.programs.coadm01c.copybook;

import java.util.Arrays;
import java.util.List;

/**
 * COADM01 (cpy-bms) - 01 COADM1AO REDEFINES COADM1AI, the symbolic output map for COADM1A. Every setter is a
 * COBOL MOVE (pad/truncate to the field length). LOW-VALUES fields are not transmitted by SEND MAP, so the
 * physical map defaults (e.g. ERRMSG COLOR=RED) apply. Only the ERRMSGC attribute byte is set by COADM01C.
 */
public final class Coadm1aOutput {

    public static final int TRNNAME_LENGTH = 4;
    public static final int TITLE_LENGTH = 40;
    public static final int CURDATE_LENGTH = 8;
    public static final int PGMNAME_LENGTH = 8;
    public static final int CURTIME_LENGTH = 8;
    public static final int OPTN_LENGTH = 40;
    public static final int OPTN_COUNT = 12;
    public static final int OPTION_LENGTH = 2;
    public static final int ERRMSG_LENGTH = 78;

    private String trnname;
    private String title01;
    private String curdate;
    private String pgmname;
    private String title02;
    private String curtime;
    private final String[] optn = new String[OPTN_COUNT];
    private String option;
    private String errmsg;
    private char errmsgc;

    private Coadm1aOutput() {
    }

    /** {@code MOVE LOW-VALUES TO COADM1AO}. */
    public static Coadm1aOutput lowValues() {
        Coadm1aOutput o = new Coadm1aOutput();
        o.trnname = CobolFields.lowValues(TRNNAME_LENGTH);
        o.title01 = CobolFields.lowValues(TITLE_LENGTH);
        o.curdate = CobolFields.lowValues(CURDATE_LENGTH);
        o.pgmname = CobolFields.lowValues(PGMNAME_LENGTH);
        o.title02 = CobolFields.lowValues(TITLE_LENGTH);
        o.curtime = CobolFields.lowValues(CURTIME_LENGTH);
        Arrays.fill(o.optn, CobolFields.lowValues(OPTN_LENGTH));
        o.option = CobolFields.lowValues(OPTION_LENGTH);
        o.errmsg = CobolFields.lowValues(ERRMSG_LENGTH);
        o.errmsgc = CobolFields.LOW_VALUE;
        return o;
    }

    /** COADM1AO after RECEIVE MAP INTO(COADM1AI): the output fields share storage with the input fields. */
    public static Coadm1aOutput fromInput(Coadm1aInput in, Coadm1aOutput before) {
        Coadm1aOutput o = before.copy();
        o.trnname = in.trnname();
        o.title01 = in.title01();
        o.curdate = in.curdate();
        o.pgmname = in.pgmname();
        o.title02 = in.title02();
        o.curtime = in.curtime();
        for (int i = 0; i < OPTN_COUNT; i++) {
            o.optn[i] = in.optn().get(i);
        }
        o.option = in.option();
        o.errmsg = in.errmsg();
        return o;
    }

    public Coadm1aOutput copy() {
        Coadm1aOutput o = new Coadm1aOutput();
        o.trnname = trnname;
        o.title01 = title01;
        o.curdate = curdate;
        o.pgmname = pgmname;
        o.title02 = title02;
        o.curtime = curtime;
        System.arraycopy(optn, 0, o.optn, 0, OPTN_COUNT);
        o.option = option;
        o.errmsg = errmsg;
        o.errmsgc = errmsgc;
        return o;
    }

    public String trnname() { return trnname; }
    public void setTrnname(String v) { trnname = CobolFields.alnum(v, TRNNAME_LENGTH); }
    public String title01() { return title01; }
    public void setTitle01(String v) { title01 = CobolFields.alnum(v, TITLE_LENGTH); }
    public String curdate() { return curdate; }
    public void setCurdate(String v) { curdate = CobolFields.alnum(v, CURDATE_LENGTH); }
    public String pgmname() { return pgmname; }
    public void setPgmname(String v) { pgmname = CobolFields.alnum(v, PGMNAME_LENGTH); }
    public String title02() { return title02; }
    public void setTitle02(String v) { title02 = CobolFields.alnum(v, TITLE_LENGTH); }
    public String curtime() { return curtime; }
    public void setCurtime(String v) { curtime = CobolFields.alnum(v, CURTIME_LENGTH); }

    /** OPTNnnnO, 1-based (OPTN001O .. OPTN012O). */
    public String optn(int idx) { return optn[idx - 1]; }
    public void setOptn(int idx, String v) { optn[idx - 1] = CobolFields.alnum(v, OPTN_LENGTH); }
    public List<String> optn() { return List.of(optn); }

    public String option() { return option; }
    public void setOption(String v) { option = CobolFields.alnum(v, OPTION_LENGTH); }
    public String errmsg() { return errmsg; }
    public void setErrmsg(String v) { errmsg = CobolFields.alnum(v, ERRMSG_LENGTH); }

    /** ERRMSGC colour attribute byte ({@link Dfhbmsca#DFHDFCOL} / LOW-VALUE = map default RED). */
    public char errmsgc() { return errmsgc; }
    public void setErrmsgc(Dfhbmsca colour) { errmsgc = colour.code; }
}
