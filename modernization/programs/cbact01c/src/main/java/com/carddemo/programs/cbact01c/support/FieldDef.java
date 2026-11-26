package com.carddemo.programs.cbact01c.support;

/** Byte offset/length of one elementary item in a fixed-width record, plus its PIC attributes. */
public record FieldDef(String cobolName, int offset, int length, Kind kind, int intDigits, int scale) {

    public enum Kind {
        /** PIC X(n). */
        ALPHANUMERIC,
        /** PIC 9(n)[V9(m)] DISPLAY. */
        UNSIGNED_DISPLAY,
        /** PIC S9(n)[V9(m)] DISPLAY, trailing overpunched sign. */
        SIGNED_DISPLAY,
        /** PIC S9(n)[V9(m)] COMP-3. */
        PACKED
    }

    public static FieldDef x(String name, int offset, int length) {
        return new FieldDef(name, offset, length, Kind.ALPHANUMERIC, 0, 0);
    }

    public static FieldDef n(String name, int offset, int digits) {
        return new FieldDef(name, offset, digits, Kind.UNSIGNED_DISPLAY, digits, 0);
    }

    public static FieldDef sn(String name, int offset, int intDigits, int scale) {
        return new FieldDef(name, offset, intDigits + scale, Kind.SIGNED_DISPLAY, intDigits, scale);
    }

    public static FieldDef packed(String name, int offset, int intDigits, int scale) {
        return new FieldDef(name, offset, PackedDecimal.byteLength(intDigits + scale), Kind.PACKED, intDigits, scale);
    }

    public int end() {
        return offset + length;
    }
}
