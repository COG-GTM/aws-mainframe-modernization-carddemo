package com.carddemo.programs.cousr00c.copybook;

import java.util.ArrayList;
import java.util.List;

/**
 * COUSR00 (app/cpy-bms/COUSR00.CPY) - symbolic map COUSR0AI / COUSR0AO for map COUSR0A.
 * Field order and lengths follow the BMS source (app/bms/COUSR00.bms); every field is FSET.
 */
public final class Cousr00Map {

    public static final String MAPSET = "COUSR00";
    public static final String MAP = "COUSR0A";
    public static final int ROWS = 10;

    public final BmsField trnname = new BmsField("TRNNAME", 4);
    public final BmsField title01 = new BmsField("TITLE01", 40);
    public final BmsField curdate = new BmsField("CURDATE", 8);
    public final BmsField pgmname = new BmsField("PGMNAME", 8);
    public final BmsField title02 = new BmsField("TITLE02", 40);
    public final BmsField curtime = new BmsField("CURTIME", 8);
    public final BmsField pagenum = new BmsField("PAGENUM", 8);
    public final BmsField usridin = new BmsField("USRIDIN", 8);
    /** SEL0001..SEL0010 PIC X(1). */
    public final BmsField[] sel = new BmsField[ROWS];
    /** USRID01..USRID10 PIC X(8). */
    public final BmsField[] usrid = new BmsField[ROWS];
    /** FNAME01..FNAME10 PIC X(20). */
    public final BmsField[] fname = new BmsField[ROWS];
    /** LNAME01..LNAME10 PIC X(20). */
    public final BmsField[] lname = new BmsField[ROWS];
    /** UTYPE01..UTYPE10 PIC X(1). */
    public final BmsField[] utype = new BmsField[ROWS];
    public final BmsField errmsg = new BmsField("ERRMSG", 78);

    public Cousr00Map() {
        for (int i = 0; i < ROWS; i++) {
            String n = String.format("%02d", i + 1);
            sel[i] = new BmsField("SEL00" + n, 1);
            usrid[i] = new BmsField("USRID" + n, 8);
            fname[i] = new BmsField("FNAME" + n, 20);
            lname[i] = new BmsField("LNAME" + n, 20);
            utype[i] = new BmsField("UTYPE" + n, 1);
        }
    }

    /** All fields in map (screen) order. */
    public List<BmsField> fields() {
        List<BmsField> all = new ArrayList<>(List.of(trnname, title01, curdate, pgmname, title02, curtime,
                pagenum, usridin));
        for (int i = 0; i < ROWS; i++) {
            all.add(sel[i]);
            all.add(usrid[i]);
            all.add(fname[i]);
            all.add(lname[i]);
            all.add(utype[i]);
        }
        all.add(errmsg);
        return all;
    }

    /** MOVE LOW-VALUES TO COUSR0AO. */
    public void lowValues() {
        fields().forEach(BmsField::lowValues);
    }

    /** Field that receives the cursor on SEND MAP ... CURSOR: first field whose L is -1. */
    public String cursorField() {
        return fields().stream().filter(f -> f.getL() == -1).map(BmsField::name).findFirst().orElse(null);
    }
}
