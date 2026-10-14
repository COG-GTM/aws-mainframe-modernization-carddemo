package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.RecordLayout;

/** CSMSG02Y {@code ABEND-DATA} (134 bytes) - every field has VALUE SPACES (see {@link #initial()}). */
public final class AbendData extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("ABEND-DATA");

    /** ABEND-CODE PIC X(4). */
    public static final AlphanumericField CODE = B.x("ABEND-CODE", 4);

    /** ABEND-CULPRIT PIC X(8). */
    public static final AlphanumericField CULPRIT = B.x("ABEND-CULPRIT", 8);

    /** ABEND-REASON PIC X(50). */
    public static final AlphanumericField REASON = B.x("ABEND-REASON", 50);

    /** ABEND-MSG PIC X(72). */
    public static final AlphanumericField MSG = B.x("ABEND-MSG", 72);

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public AbendData() {
        super(LENGTH);
    }

    public static AbendData of(byte[] bytes) {
        AbendData r = new AbendData();
        r.setBytes(bytes);
        return r;
    }

    public static AbendData of(String record) {
        AbendData r = new AbendData();
        r.move(record);
        return r;
    }

    /** ABEND-DATA as declared: all fields VALUE SPACES. */
    public static AbendData initial() {
        AbendData r = new AbendData();
        r.initializeSpaces();
        return r;
    }

    public String code() {
        return CODE.get(this);
    }

    public String culprit() {
        return CULPRIT.get(this);
    }

    public String reason() {
        return REASON.get(this);
    }

    public String msg() {
        return MSG.get(this);
    }
}
