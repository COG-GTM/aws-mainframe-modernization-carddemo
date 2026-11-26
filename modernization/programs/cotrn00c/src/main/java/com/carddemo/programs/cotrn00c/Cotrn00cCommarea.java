package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrn00c.copybook.CobolText;

/** COTRN00C's CARDDEMO-COMMAREA: COCOM01Y (160 bytes) followed by CDEMO-CT00-INFO (58 bytes). */
public final class Cotrn00cCommarea {

    public static final int LENGTH = CarddemoCommarea.LENGTH + Ct00Info.LENGTH;

    private final CarddemoCommarea general;
    private final Ct00Info ct00;

    private Cotrn00cCommarea(CarddemoCommarea general, Ct00Info ct00) {
        this.general = general;
        this.ct00 = ct00;
    }

    /**
     * MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA: an alphanumeric group move, so a shorter
     * COMMAREA (e.g. the 160-byte one from the menu) is padded with spaces and a longer one truncated.
     */
    public static Cotrn00cCommarea fromBytes(String dfhCommarea) {
        String data = CobolText.fit(dfhCommarea, LENGTH);
        return new Cotrn00cCommarea(CarddemoCommarea.decode(data, 0),
                Ct00Info.decode(data, CarddemoCommarea.LENGTH));
    }

    /** Working-storage COMMAREA before any MOVE (no VALUE clauses except NEXT-PAGE-FLG 'N'). */
    public static Cotrn00cCommarea initial() {
        Cotrn00cCommarea c = fromBytes("");
        c.ct00.setNextPageNo();
        return c;
    }

    public String toBytes() {
        return general.encode() + ct00.encode();
    }

    public Cotrn00cCommarea copy() {
        return fromBytes(toBytes());
    }

    public CarddemoCommarea general() {
        return general;
    }

    public Ct00Info ct00() {
        return ct00;
    }
}
