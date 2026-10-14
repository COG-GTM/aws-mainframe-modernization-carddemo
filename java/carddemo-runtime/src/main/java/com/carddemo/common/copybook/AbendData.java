package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.RecordLayout;

/** CSMSG02Y {@code ABEND-DATA} (134 bytes): work area of the online programs' ABEND-ROUTINE. */
public final class AbendData extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("ABEND-DATA");

    /** {@code ABEND-CODE} PIC X(4) VALUE SPACES. */
    public static final AlphanumericField CODE = B.x("ABEND-CODE", 4);
    /** {@code ABEND-CULPRIT} PIC X(8) VALUE SPACES. */
    public static final AlphanumericField CULPRIT = B.x("ABEND-CULPRIT", 8);
    /** {@code ABEND-REASON} PIC X(50) VALUE SPACES. */
    public static final AlphanumericField REASON = B.x("ABEND-REASON", 50);
    /** {@code ABEND-MSG} PIC X(72) VALUE SPACES. */
    public static final AlphanumericField MSG = B.x("ABEND-MSG", 72);

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    /** New ABEND-DATA with its VALUE clauses applied (all SPACES). */
    public AbendData() {
        super(LENGTH);
        initializeSpaces();
    }

    public String code() {
        return CODE.get(this);
    }

    public void setCode(String v) {
        CODE.set(this, v);
    }

    public String culprit() {
        return CULPRIT.get(this);
    }

    public void setCulprit(String v) {
        CULPRIT.set(this, v);
    }

    public String reason() {
        return REASON.get(this);
    }

    public void setReason(String v) {
        REASON.set(this, v);
    }

    public String msg() {
        return MSG.get(this);
    }

    public void setMsg(String v) {
        MSG.set(this, v);
    }
}
