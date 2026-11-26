package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.FixedRecord;

/**
 * TTYP-UPDATE-RECORD (COTRTUPC WORKING-STORAGE, 60 bytes) - same layout as the TRANTYPE VSAM record
 * (app/data/ASCII/trantype.txt): TYPE X(02), TYPE-DESC X(50), FILLER X(08).
 */
public final class TtypUpdateRecord extends FixedRecord {

    public static final int LENGTH = 60;

    public TtypUpdateRecord() {
        super(LENGTH);
    }

    public static TtypUpdateRecord parse(String line) {
        TtypUpdateRecord r = new TtypUpdateRecord();
        r.fromFixed(com.carddemo.programs.cotrtupc.support.CobolText.pic(line, LENGTH));
        return r;
    }

    public String ttupUpdateTtypType() { return get(0, 2); }
    public void ttupUpdateTtypType(String v) { set(0, 2, v); }
    public String ttupUpdateTtypTypeDesc() { return get(2, 50); }
    public void ttupUpdateTtypTypeDesc(String v) { set(2, 50, v); }
    public String filler() { return get(52, 8); }
}
