package com.carddemo.programs.cousr02c.copybook;

import java.util.List;
import java.util.Optional;

/**
 * COUSR02 (app/cpy-bms/COUSR02.CPY) - symbolic map {@code COUSR2AI} / {@code COUSR2AO REDEFINES COUSR2AI} for
 * mapset COUSR02, map COUSR2A (app/bms/COUSR02.bms, 24x80). The 12-byte TIOAPFX filler is not modelled.
 */
public final class Cousr2aMap {

    public final MapField trnname = new MapField("TRNNAME", 4);
    public final MapField title01 = new MapField("TITLE01", 40);
    public final MapField curdate = new MapField("CURDATE", 8);
    public final MapField pgmname = new MapField("PGMNAME", 8);
    public final MapField title02 = new MapField("TITLE02", 40);
    public final MapField curtime = new MapField("CURTIME", 8);
    public final MapField usridin = new MapField("USRIDIN", 8);
    public final MapField fname = new MapField("FNAME", 20);
    public final MapField lname = new MapField("LNAME", 20);
    public final MapField passwd = new MapField("PASSWD", 8);
    public final MapField usrtype = new MapField("USRTYPE", 1);
    public final MapField errmsg = new MapField("ERRMSG", 78);

    /** Fields in map (screen) order. */
    public List<MapField> fields() {
        return List.of(trnname, title01, curdate, pgmname, title02, curtime, usridin, fname, lname, passwd, usrtype,
                errmsg);
    }

    /** MOVE LOW-VALUES TO COUSR2AO. */
    public void lowValues() {
        fields().forEach(MapField::lowValues);
    }

    /**
     * Field on which SEND MAP ... CURSOR places the cursor: the first field, in map order, whose length is -1;
     * otherwise the field defined with IC on the BMS macro (USRIDIN).
     */
    public MapField cursorField() {
        Optional<MapField> first = fields().stream().filter(f -> f.getLength() == MapField.CURSOR).findFirst();
        return first.orElse(usridin);
    }
}
