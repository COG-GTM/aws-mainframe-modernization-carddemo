package com.carddemo.programs.cousr03c.copybook;

/**
 * COUSR03 symbolic map (app/cpy-bms/COUSR03.CPY): 01 COUSR3AI with 01 COUSR3AO REDEFINES COUSR3AI.
 * One 324-byte buffer backs both views, so input (L/F/A/I) and output (C/P/H/V/O) suffixes overlap
 * exactly as in COBOL: per field, input is L(2) F|A(1) FILLER(4) I(n); output is FILLER(3) C P H V O(n).
 */
public final class Cousr03Map extends FixedRecord {

    /** Named fields of map COUSR3A, in BMS order. */
    public enum Field {
        TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
        USRIDIN(8), FNAME(20), LNAME(20), USRTYPE(1), ERRMSG(78);

        public final int size;
        final int offset;

        Field(int size) {
            this.size = size;
            this.offset = Offsets.next(size);
        }

        private static final class Offsets {
            private static int cursor = 12;

            static int next(int size) {
                int o = cursor;
                cursor += 7 + size;
                return o;
            }
        }
    }

    public static final int LENGTH = 324;

    public Cousr03Map() {
        super(LENGTH);
    }

    // ---- COUSR3AI view ----
    /** {field}L COMP S9(4). */
    public short l(Field f) { return getShort(f.offset); }
    public void setL(Field f, int v) { setShort(f.offset, (short) v); }
    /** {field}F PIC X. */
    public byte f(Field f) { return getByte(f.offset + 2); }
    public void setF(Field f, byte v) { setByte(f.offset + 2, v); }
    /** {field}A PIC X (REDEFINES {field}F). */
    public byte a(Field f) { return getByte(f.offset + 2); }
    public void setA(Field f, byte v) { setByte(f.offset + 2, v); }
    /** {field}I PIC X(n). */
    public String i(Field f) { return getX(f.offset + 7, f.size); }
    public void setI(Field f, String v) { setX(f.offset + 7, f.size, v); }
    public void fillI(Field f, byte v) { fillX(f.offset + 7, f.size, v); }

    // ---- COUSR3AO view ----
    /** {field}C PIC X (colour). */
    public byte c(Field f) { return getByte(f.offset + 3); }
    public void setC(Field f, byte v) { setByte(f.offset + 3, v); }
    /** {field}P PIC X (programmed symbols). */
    public byte p(Field f) { return getByte(f.offset + 4); }
    public void setP(Field f, byte v) { setByte(f.offset + 4, v); }
    /** {field}H PIC X (highlight). */
    public byte h(Field f) { return getByte(f.offset + 5); }
    public void setH(Field f, byte v) { setByte(f.offset + 5, v); }
    /** {field}V PIC X (validation). */
    public byte v(Field f) { return getByte(f.offset + 6); }
    public void setV(Field f, byte v) { setByte(f.offset + 6, v); }
    /** {field}O PIC X(n) (same storage as {field}I). */
    public String o(Field f) { return getX(f.offset + 7, f.size); }
    public void setO(Field f, String v) { setX(f.offset + 7, f.size, v); }
}
