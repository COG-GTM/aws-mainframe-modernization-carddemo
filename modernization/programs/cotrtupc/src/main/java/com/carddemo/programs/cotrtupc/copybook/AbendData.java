package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;
import com.carddemo.programs.cotrtupc.support.FixedRecord;

/** CSMSG02Y - ABEND-DATA (134 bytes, every field VALUE SPACES). */
public final class AbendData extends FixedRecord {

    public static final int LENGTH = 134;

    static final int CODE = 0;       // X(4)
    static final int CULPRIT = 4;    // X(8)
    static final int REASON = 12;    // X(50)
    static final int MSG = 62;       // X(72)

    public AbendData() {
        super(LENGTH);
        fill(0, LENGTH, ' ');
    }

    public String abendCode() { return get(CODE, 4); }
    public void abendCode(String v) { set(CODE, 4, v); }
    public String abendCulprit() { return get(CULPRIT, 8); }
    public void abendCulprit(String v) { set(CULPRIT, 8, v); }
    public String abendReason() { return get(REASON, 50); }
    public void abendReason(String v) { set(REASON, 50, v); }
    public String abendMsg() { return get(MSG, 72); }
    public void abendMsg(String v) { set(MSG, 72, v); }
    public boolean isAbendMsgLowValues() { return CobolText.isLowValues(abendMsg()); }

    public AbendData copy() {
        AbendData c = new AbendData();
        c.fromFixed(toFixed());
        return c;
    }
}
