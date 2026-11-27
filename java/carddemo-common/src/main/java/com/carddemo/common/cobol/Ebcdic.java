package com.carddemo.common.cobol;

/** EBCDIC (IBM-037) conversion helpers. All conversions are single-byte and length preserving. */
public final class Ebcdic {

    public static final byte SPACE = 0x40;
    public static final byte LOW_VALUE = 0x00;
    public static final byte HIGH_VALUE = (byte) 0xFF;

    private static final byte[] TO_ASCII = new byte[256];
    private static final byte[] TO_EBCDIC = new byte[256];

    static {
        byte[] all = new byte[256];
        for (int i = 0; i < 256; i++) {
            all[i] = (byte) i;
        }
        String unicode = DataEncoding.EBCDIC.decode(all, 0, 256);
        // Build a bijection: bytes whose Unicode mapping is outside Latin-1 or already taken (IBM-037 decodes
        // both NL x'15' and LF x'25' to U+000A in the JDK) get the remaining unused Latin-1 code points.
        boolean[] taken = new boolean[256];
        boolean[] assigned = new boolean[256];
        for (int i = 0; i < 256; i++) {
            char c = unicode.charAt(i);
            if (c <= 0xFF && !taken[c]) {
                taken[c] = true;
                assigned[i] = true;
                TO_ASCII[i] = (byte) c;
                TO_EBCDIC[c] = (byte) i;
            }
        }
        int free = 0;
        for (int i = 0; i < 256; i++) {
            if (!assigned[i]) {
                while (taken[free]) {
                    free++;
                }
                taken[free] = true;
                TO_ASCII[i] = (byte) free;
                TO_EBCDIC[free] = (byte) i;
            }
        }
    }

    private Ebcdic() {
    }

    public static String toUnicode(byte[] ebcdic) {
        return DataEncoding.EBCDIC.decode(ebcdic, 0, ebcdic.length);
    }

    public static byte[] fromUnicode(String text) {
        return DataEncoding.EBCDIC.encode(text);
    }

    /** Converts EBCDIC bytes to Latin-1 bytes; only valid for DISPLAY data (never for COMP/COMP-3 fields). */
    public static byte[] toAscii(byte[] ebcdic) {
        return toAscii(ebcdic, 0, ebcdic.length);
    }

    public static byte[] toAscii(byte[] ebcdic, int offset, int length) {
        byte[] out = new byte[length];
        for (int i = 0; i < length; i++) {
            out[i] = TO_ASCII[ebcdic[offset + i] & 0xFF];
        }
        return out;
    }

    /** Converts Latin-1 bytes to EBCDIC bytes; only valid for DISPLAY data (never for COMP/COMP-3 fields). */
    public static byte[] toEbcdic(byte[] ascii) {
        return toEbcdic(ascii, 0, ascii.length);
    }

    public static byte[] toEbcdic(byte[] ascii, int offset, int length) {
        byte[] out = new byte[length];
        for (int i = 0; i < length; i++) {
            out[i] = TO_EBCDIC[ascii[offset + i] & 0xFF];
        }
        return out;
    }

    public static boolean isSpaces(byte[] ebcdic, int offset, int length) {
        return all(ebcdic, offset, length, SPACE);
    }

    public static boolean isLowValues(byte[] buf, int offset, int length) {
        return all(buf, offset, length, LOW_VALUE);
    }

    public static boolean isHighValues(byte[] buf, int offset, int length) {
        return all(buf, offset, length, HIGH_VALUE);
    }

    private static boolean all(byte[] buf, int offset, int length, byte value) {
        for (int i = offset; i < offset + length; i++) {
            if (buf[i] != value) {
                return false;
            }
        }
        return true;
    }
}
