package com.carddemo.interest.io;

import java.nio.charset.Charset;

/**
 * EBCDIC &lt;-&gt; ASCII conversion for the CardDemo mainframe datasets.
 *
 * <p>The static dataset dumps under {@code app/data/EBCDIC} are encoded in IBM code page 037
 * (US/Canada EBCDIC), the default code page of the z/OS systems CardDemo was exported from.
 * Code page 037 is single byte, so a character offset in a copybook record layout is also a
 * byte offset, which the {@link RecordLayout} field addressing relies on.
 */
public final class EbcdicText {

    /** IBM-037: US/Canada EBCDIC, the code page of the CardDemo dataset dumps. */
    public static final Charset CP037 = Charset.forName("IBM037");

    /** EBCDIC encoding of the space character (ASCII 0x20 is EBCDIC 0x40). */
    public static final byte EBCDIC_SPACE = 0x40;

    private EbcdicText() {
    }

    /** Converts {@code length} EBCDIC bytes starting at {@code offset} into an ASCII/Unicode string. */
    public static String toAscii(byte[] record, int offset, int length) {
        return new String(record, offset, length, CP037);
    }

    /** Converts an ASCII/Unicode string into EBCDIC bytes. */
    public static byte[] toEbcdic(String text) {
        return text.getBytes(CP037);
    }

    /** Creates a record of {@code length} bytes filled with EBCDIC spaces. */
    public static byte[] blankRecord(int length) {
        byte[] record = new byte[length];
        java.util.Arrays.fill(record, EBCDIC_SPACE);
        return record;
    }
}
