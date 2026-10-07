package com.carddemo.batch.codec;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Byte-for-byte codec for fixed-width COBOL records. Records are kept as the byte buffer COBOL would
 * hold them in (so {@code decode(encode(x)) == x} trivially and INITIALIZE/MOVE sign semantics survive);
 * the methods here read and write typed values at a {@link Field}'s position.
 *
 * <p>Text is ISO-8859-1: every byte maps to exactly one char, so trailing spaces and any non-ASCII byte
 * in the sample data round-trip unchanged.
 */
public final class FixedWidth {

    public static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    private FixedWidth() {
    }

    // ----- scalar access ----------------------------------------------------------------------

    /** PIC X: the bytes as text, trailing spaces preserved. */
    public static String text(byte[] buf, Field f) {
        return new String(buf, f.offset(), f.length(), CHARSET);
    }

    /** COBOL MOVE to PIC X: left-justified, space padded, right-truncated. */
    public static void setText(byte[] buf, Field f, String value) {
        byte[] src = value.getBytes(CHARSET);
        int n = Math.min(src.length, f.length());
        System.arraycopy(src, 0, buf, f.offset(), n);
        for (int i = n; i < f.length(); i++) {
            buf[f.offset() + i] = ' ';
        }
    }

    /** Numeric (zoned or packed) value with exactly the PIC's scale. */
    public static BigDecimal decimal(byte[] buf, Field f) {
        switch (f.usage()) {
            case ZONED:
                return ZonedDecimal.decode(buf, f.offset(), f.length(), f.scale(), f.signed());
            case PACKED:
                return PackedDecimal.decode(buf, f.offset(), f.length(), f.scale());
            default:
                throw new IllegalArgumentException(f.name() + " is not numeric");
        }
    }

    public static void setDecimal(byte[] buf, Field f, BigDecimal value) {
        switch (f.usage()) {
            case ZONED:
                ZonedDecimal.encode(buf, f.offset(), f.length(), f.scale(), f.signed(), value);
                break;
            case PACKED:
                PackedDecimal.encode(buf, f.offset(), f.length(), f.digits(), f.scale(), f.signed(), value);
                break;
            default:
                throw new IllegalArgumentException(f.name() + " is not numeric");
        }
    }

    /** PIC 9(n) as a long. */
    public static long unsigned(byte[] buf, Field f) {
        return decimal(buf, f).longValueExact();
    }

    public static void setUnsigned(byte[] buf, Field f, long value) {
        setDecimal(buf, f, BigDecimal.valueOf(value));
    }

    /** Raw bytes of a field (used for key comparison and DISPLAY of whole records). */
    public static byte[] bytes(byte[] buf, Field f) {
        byte[] out = new byte[f.length()];
        System.arraycopy(buf, f.offset(), out, 0, f.length());
        return out;
    }

    public static void setBytes(byte[] buf, Field f, byte[] value) {
        System.arraycopy(value, 0, buf, f.offset(), Math.min(value.length, f.length()));
    }

    /** What COBOL {@code DISPLAY field} prints (GnuCOBOL conventions for signed zoned items). */
    public static String display(byte[] buf, Field f) {
        switch (f.usage()) {
            case ZONED:
                return ZonedDecimal.display(buf, f.offset(), f.length(), f.signed());
            case TEXT:
            case FILLER:
            case GROUP:
                return text(buf, f);
            default:
                throw new IllegalArgumentException("DISPLAY of " + f.usage() + " is not supported");
        }
    }

    // ----- record level -----------------------------------------------------------------------

    /** COBOL {@code INITIALIZE}: numerics to (unsigned) zero, text to spaces, FILLER untouched. */
    public static void initialize(byte[] buf, Layout layout) {
        for (Field f : layout.fields()) {
            initialize(buf, f);
        }
    }

    public static void initialize(byte[] buf, Field f) {
        switch (f.usage()) {
            case TEXT:
                for (int i = 0; i < f.length(); i++) {
                    buf[f.offset() + i] = ' ';
                }
                break;
            case ZONED:
                ZonedDecimal.initialize(buf, f.offset(), f.length());
                break;
            case PACKED:
                PackedDecimal.initialize(buf, f.offset(), f.length());
                break;
            case GROUP:
                for (int i = 0; i < f.occurs(); i++) {
                    for (Field c : f.occurrence(i).children()) {
                        initialize(buf, c);
                    }
                }
                break;
            case FILLER:
            default:
                break;
        }
    }

    /** A fresh record buffer filled with spaces (GnuCOBOL's initial content for record areas). */
    public static byte[] blank(Layout layout) {
        byte[] buf = new byte[layout.length()];
        java.util.Arrays.fill(buf, (byte) ' ');
        return buf;
    }

    /** Validates the record length and returns a defensive copy. */
    public static byte[] decode(Layout layout, byte[] raw) {
        if (raw.length != layout.length()) {
            throw new CodecException(layout.name() + " needs " + layout.length() + " bytes, got " + raw.length);
        }
        return raw.clone();
    }

    /**
     * Decodes every non-FILLER field into a name -> value map: text as {@link String}, numerics as
     * {@link BigDecimal} with the PIC scale, OCCURS groups as a {@code List<Map>} with one entry per
     * occurrence. This is the same shape the test-harness {@code records.py} produces.
     */
    public static Map<String, Object> toMap(byte[] buf, Layout layout) {
        return toMap(buf, layout.fields());
    }

    private static Map<String, Object> toMap(byte[] buf, List<Field> fields) {
        Map<String, Object> out = new LinkedHashMap<>();
        for (Field f : fields) {
            switch (f.usage()) {
                case FILLER:
                    break;
                case TEXT:
                    out.put(f.name(), text(buf, f));
                    break;
                case ZONED:
                case PACKED:
                    out.put(f.name(), decimal(buf, f));
                    break;
                case GROUP:
                    List<Map<String, Object>> occ = new ArrayList<>(f.occurs());
                    for (int i = 0; i < f.occurs(); i++) {
                        occ.add(toMap(buf, f.occurrence(i).children()));
                    }
                    out.put(f.name(), f.occurs() > 1 ? occ : occ.get(0));
                    break;
                default:
                    break;
            }
        }
        return out;
    }
}
