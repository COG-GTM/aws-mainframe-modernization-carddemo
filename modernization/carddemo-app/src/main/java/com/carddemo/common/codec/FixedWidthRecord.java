package com.carddemo.common.codec;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Objects;

/**
 * A mutable fixed-length record image (a COBOL record area) with typed access to its items.
 *
 * <p>Text follows ADR-0003: {@link #setString} right-pads with spaces and rejects overflow, {@link #moveString}
 * truncates like a COBOL MOVE. Numbers follow ADR-0004/0005: {@link #setDecimal} truncates excess fraction
 * digits ({@code RoundingMode.DOWN}) but rejects values whose integer part does not fit or that are negative
 * for an unsigned item; {@link #moveDecimal} is the full COBOL MOVE (high-order truncation, sign dropped).
 * A numeric item holding only LOW-VALUES (an initialised-but-empty slot) reads as {@code null} through
 * {@link #get(Field)}.
 */
public final class FixedWidthRecord {

    private final byte[] image;
    private final RecordEncoding encoding;
    private final RecordLayout layout;

    public FixedWidthRecord(byte[] image, RecordEncoding encoding) {
        this(null, image, encoding);
    }

    public FixedWidthRecord(RecordLayout layout, byte[] image, RecordEncoding encoding) {
        this.layout = layout;
        this.image = Objects.requireNonNull(image, "image");
        this.encoding = Objects.requireNonNull(encoding, "encoding");
        if (layout != null && layout.length() != image.length) {
            throw new RecordFormatException(layout.name() + " is " + layout.length() + " bytes, image is "
                    + image.length);
        }
    }

    /** A record of spaces, as after {@code MOVE SPACES TO record}. */
    public static FixedWidthRecord spaces(RecordLayout layout, RecordEncoding encoding) {
        byte[] image = new byte[layout.length()];
        Arrays.fill(image, encoding.space());
        return new FixedWidthRecord(layout, image, encoding);
    }

    /** A record from one line of a line-sequential file: short lines are space padded, long lines rejected. */
    public static FixedWidthRecord fromLine(RecordLayout layout, String line, RecordEncoding encoding) {
        if (line.length() > layout.length()) {
            throw new RecordFormatException(layout.name() + " line is " + line.length() + " characters, record is "
                    + layout.length());
        }
        return new FixedWidthRecord(layout, encoding.encode(pad(line, layout.length())), encoding);
    }

    public byte[] bytes() {
        return image.clone();
    }

    public int length() {
        return image.length;
    }

    public RecordEncoding encoding() {
        return encoding;
    }

    public RecordLayout layout() {
        return layout;
    }

    /** The whole record as characters (packed/binary bytes come out as their code points). */
    public String text() {
        return encoding.decode(image, 0, image.length);
    }

    public FixedWidthRecord copy() {
        return new FixedWidthRecord(layout, image.clone(), encoding);
    }

    /** The same bytes viewed through another layout of the same length (a record-level REDEFINES). */
    public FixedWidthRecord as(RecordLayout other) {
        return new FixedWidthRecord(other, image, encoding);
    }

    public Field field(String name, String... qualifiers) {
        if (layout == null) {
            throw new IllegalStateException("record has no layout; use Field accessors");
        }
        return layout.field(name, qualifiers);
    }

    public String getString(Field f) {
        check(f);
        return encoding.decode(image, f.offset(), f.size());
    }

    public String getString(String name) {
        return getString(field(name));
    }

    /** The text with trailing spaces removed (ADR-0003 read side). */
    public String getTrimmed(Field f) {
        return getString(f).stripTrailing();
    }

    public BigDecimal getDecimal(Field f) {
        check(f);
        requireNumeric(f);
        return switch (f.usage()) {
            case DISPLAY -> CobolNumeric.decodeZoned(getString(f), f.scale(), f.signed());
            case PACKED -> CobolNumeric.decodePacked(image, f.offset(), f.size(), f.scale());
            case BINARY -> CobolNumeric.decodeBinary(image, f.offset(), f.size(), f.scale(), f.signed());
        };
    }

    public BigDecimal getDecimal(String name) {
        return getDecimal(field(name));
    }

    public long getLong(Field f) {
        if (f.scale() != 0) {
            throw new IllegalArgumentException(f.name() + " has " + f.scale() + " decimal places; use getDecimal");
        }
        return getDecimal(f).longValueExact();
    }

    public long getLong(String name) {
        return getLong(field(name));
    }

    /**
     * String for alphanumeric, edited and group items; BigDecimal for numeric ones, or null for a DISPLAY or
     * COMP-3 item holding LOW-VALUES (all-zero bytes are a valid binary zero, so COMP items never read as null).
     */
    public Object get(Field f) {
        if (!f.isNumeric()) {
            return getString(f);
        }
        return f.usage() != Usage.BINARY && isLowValues(f) ? null : getDecimal(f);
    }

    public Object get(String name) {
        return get(field(name));
    }

    public void setString(Field f, String value) {
        check(f);
        if (value.length() > f.size()) {
            throw new RecordFormatException(f.name() + " holds " + f.size() + " characters, got " + value.length()
                    + ": '" + value + "'");
        }
        moveString(f, value);
    }

    public void setString(String name, String value) {
        setString(field(name), value);
    }

    /** {@code MOVE value TO item} for an alphanumeric receiver: pad or truncate on the right. */
    public void moveString(Field f, String value) {
        check(f);
        if (f.isNumeric()) {
            throw new IllegalArgumentException(f.name() + " is numeric; use setDecimal");
        }
        String fitted = value.length() > f.size() ? value.substring(0, f.size()) : pad(value, f.size());
        System.arraycopy(encoding.encode(fitted), 0, image, f.offset(), f.size());
    }

    public void setDecimal(Field f, BigDecimal value) {
        check(f);
        if (f.isNumeric()) {
            if (!CobolNumeric.fits(value, f.digits(), f.scale())) {
                throw new RecordFormatException(f.name() + " (PIC " + f.picture().text() + ") cannot hold " + value);
            }
            if (!f.signed() && value.signum() < 0) {
                throw new RecordFormatException(f.name() + " is unsigned, got " + value);
            }
        }
        moveDecimal(f, value);
    }

    public void setDecimal(String name, BigDecimal value) {
        setDecimal(field(name), value);
    }

    public void setLong(Field f, long value) {
        setDecimal(f, BigDecimal.valueOf(value));
    }

    public void setLong(String name, long value) {
        setLong(field(name), value);
    }

    /** {@code MOVE value TO item} for a numeric or numeric-edited receiver. */
    public void moveDecimal(Field f, BigDecimal value) {
        check(f);
        if (f.isNumericEdited()) {
            moveString(f, NumericEdited.format(value, f.picture().text()));
            return;
        }
        requireNumeric(f);
        switch (f.usage()) {
            case DISPLAY -> System.arraycopy(encoding.encode(
                    CobolNumeric.encodeZoned(value, f.digits(), f.scale(), f.signed())), 0, image, f.offset(), f.size());
            case PACKED -> CobolNumeric.encodePacked(image, f.offset(), f.size(), f.digits(), f.scale(), f.signed(),
                    value);
            case BINARY -> CobolNumeric.encodeBinary(image, f.offset(), f.size(), f.digits(), f.scale(), f.signed(),
                    value);
        }
    }

    /** Stores a value as returned by {@link #get(Field)}: null means LOW-VALUES. */
    public void set(Field f, Object value) {
        if (value == null) {
            fill(f, (byte) 0x00);
        } else if (value instanceof BigDecimal d) {
            setDecimal(f, d);
        } else if (value instanceof Long || value instanceof Integer || value instanceof Short) {
            setLong(f, ((Number) value).longValue());
        } else if (value instanceof String s) {
            setString(f, s);
        } else {
            throw new IllegalArgumentException("unsupported value type " + value.getClass().getName());
        }
    }

    public void set(String name, Object value) {
        set(field(name), value);
    }

    public void fill(Field f, byte b) {
        check(f);
        Arrays.fill(image, f.offset(), f.offset() + f.size(), b);
    }

    public boolean isSpaces(Field f) {
        return allBytes(f, encoding.space());
    }

    public boolean isLowValues(Field f) {
        return allBytes(f, (byte) 0x00);
    }

    /** The COBOL {@code IS NUMERIC} class test. */
    public boolean isNumeric(Field f) {
        check(f);
        if (!f.isNumeric()) {
            String s = getString(f);
            return !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
        }
        try {
            getDecimal(f);
            return true;
        } catch (RecordFormatException e) {
            return false;
        }
    }

    private boolean allBytes(Field f, byte b) {
        check(f);
        for (int i = f.offset(); i < f.offset() + f.size(); i++) {
            if (image[i] != b) {
                return false;
            }
        }
        return true;
    }

    private void check(Field f) {
        if (f.offset() < 0 || f.offset() + f.size() > image.length) {
            throw new IndexOutOfBoundsException(f.name() + " @" + f.offset() + "+" + f.size()
                    + " outside a " + image.length + "-byte record");
        }
    }

    private static void requireNumeric(Field f) {
        if (!f.isNumeric()) {
            throw new IllegalArgumentException(f.name() + " is not numeric");
        }
    }

    private static String pad(String s, int width) {
        return s.length() >= width ? s : s + " ".repeat(width - s.length());
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof FixedWidthRecord r && Arrays.equals(r.image, image) && r.encoding.equals(encoding);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(image);
    }

    @Override
    public String toString() {
        return (layout == null ? "record" : layout.name()) + "[" + text() + "]";
    }
}
