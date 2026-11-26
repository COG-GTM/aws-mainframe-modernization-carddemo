package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * COTRTUP (cpy-bms) - symbolic map CTRTUPAI / CTRTUPAO (REDEFINES). Each BMS field carries the input
 * view (L length, F/A attribute, I data) and the output view (A attribute, C colour, P, H, V, O data),
 * which share storage exactly as in the copybook.
 */
public final class CtrtupaMap {

    /** One BMS field: xxxL, xxxF/xxxA, xxxC, xxxP, xxxH, xxxV, xxxI/xxxO. */
    public static final class BmsField {
        public final String name;
        public final int size;
        /** xxxL COMP S9(4); -1 requests symbolic cursor positioning. */
        public short length;
        /** xxxF / xxxA. */
        public char attribute;
        /** xxxC. */
        public char color;
        /** xxxP. */
        public char ps;
        /** xxxH. */
        public char hilight;
        /** xxxV. */
        public char validn;
        /** xxxI / xxxO. */
        public String data;

        BmsField(String name, int size) {
            this.name = name;
            this.size = size;
            lowValues();
        }

        void lowValues() {
            length = 0;
            attribute = CobolText.LOW_VALUE;
            color = CobolText.LOW_VALUE;
            ps = CobolText.LOW_VALUE;
            hilight = CobolText.LOW_VALUE;
            validn = CobolText.LOW_VALUE;
            data = CobolText.lowValues(size);
        }

        /** MOVE x TO xxxO / xxxI (alphanumeric). */
        public void move(String v) {
            data = CobolText.pic(v, size);
        }

        public void moveLowValues() {
            data = CobolText.lowValues(size);
        }

        /** Data as rendered on a 3270 (LOW-VALUES are not transmitted and display as blanks). */
        public String displayText() {
            return data.replace(CobolText.LOW_VALUE, ' ');
        }

        BmsField copy() {
            BmsField f = new BmsField(name, size);
            f.length = length;
            f.attribute = attribute;
            f.color = color;
            f.ps = ps;
            f.hilight = hilight;
            f.validn = validn;
            f.data = data;
            return f;
        }
    }

    public final BmsField trnname = new BmsField("TRNNAME", 4);
    public final BmsField title01 = new BmsField("TITLE01", 40);
    public final BmsField curdate = new BmsField("CURDATE", 8);
    public final BmsField pgmname = new BmsField("PGMNAME", 8);
    public final BmsField title02 = new BmsField("TITLE02", 40);
    public final BmsField curtime = new BmsField("CURTIME", 8);
    public final BmsField trtypcd = new BmsField("TRTYPCD", 2);
    public final BmsField trtydsc = new BmsField("TRTYDSC", 50);
    public final BmsField infomsg = new BmsField("INFOMSG", 45);
    public final BmsField errmsg = new BmsField("ERRMSG", 78);
    public final BmsField fkeys = new BmsField("FKEYS", 21);
    public final BmsField fkey04 = new BmsField("FKEY04", 9);
    public final BmsField fkey05 = new BmsField("FKEY05", 8);
    public final BmsField fkey06 = new BmsField("FKEY06", 6);
    public final BmsField fkey12 = new BmsField("FKEY12", 10);

    public BmsField[] fields() {
        return new BmsField[] {trnname, title01, curdate, pgmname, title02, curtime, trtypcd, trtydsc,
            infomsg, errmsg, fkeys, fkey04, fkey05, fkey06, fkey12};
    }

    /** Length of CTRTUPAI in bytes: 12-byte TIOA prefix + (7 + size) per field. */
    public int byteLength() {
        int n = 12;
        for (BmsField f : fields()) {
            n += 7 + f.size;
        }
        return n;
    }

    /** MOVE LOW-VALUES TO CTRTUPAO. */
    public void lowValues() {
        for (BmsField f : fields()) {
            f.lowValues();
        }
    }

    /** Field (if any) carrying the symbolic cursor (xxxL = -1). */
    public String cursorField() {
        for (BmsField f : fields()) {
            if (f.length == -1) {
                return f.name;
            }
        }
        return null;
    }

    public CtrtupaMap copy() {
        CtrtupaMap m = new CtrtupaMap();
        BmsField[] src = fields();
        BmsField[] dst = m.fields();
        for (int i = 0; i < src.length; i++) {
            BmsField c = src[i].copy();
            dst[i].length = c.length;
            dst[i].attribute = c.attribute;
            dst[i].color = c.color;
            dst[i].ps = c.ps;
            dst[i].hilight = c.hilight;
            dst[i].validn = c.validn;
            dst[i].data = c.data;
        }
        return m;
    }

    public Map<String, BmsField> byName() {
        Map<String, BmsField> m = new LinkedHashMap<>();
        for (BmsField f : fields()) {
            m.put(f.name, f);
        }
        return m;
    }
}
