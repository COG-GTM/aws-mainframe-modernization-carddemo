package com.carddemo.programs.cousr03c.copybook;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Byte-exact storage for a COBOL 01-level item. Alphanumeric MOVE semantics
 * (left-justify, space-pad, right-truncate) and group MOVE semantics are applied
 * so that REDEFINES and partial COMMAREA moves behave as on the mainframe.
 * Characters are held as ISO-8859-1 bytes; LOW-VALUES is 0x00, SPACES is 0x20.
 */
public abstract class FixedRecord {

    public static final byte SPACE = 0x20;
    public static final byte LOW_VALUE = 0x00;

    protected final byte[] buf;
    protected final int base;
    private final int length;

    protected FixedRecord(int length) {
        this(new byte[length], 0, length);
    }

    protected FixedRecord(byte[] buf, int base, int length) {
        this.buf = buf;
        this.base = base;
        this.length = length;
    }

    public final int length() {
        return length;
    }

    public final byte[] toBytes() {
        return Arrays.copyOfRange(buf, base, base + length);
    }

    /** Group MOVE: copy left-to-right, pad the rest with spaces, truncate overflow. */
    public final void moveFrom(byte[] source) {
        int n = Math.min(source.length, length);
        System.arraycopy(source, 0, buf, base, n);
        Arrays.fill(buf, base + n, base + length, SPACE);
    }

    /** MOVE SPACES / LOW-VALUES / HIGH-VALUES TO the whole record. */
    public final void fill(byte value) {
        Arrays.fill(buf, base, base + length, value);
    }

    protected final String getX(int off, int len) {
        return new String(buf, base + off, len, StandardCharsets.ISO_8859_1);
    }

    protected final void setX(int off, int len, String value) {
        byte[] src = value == null ? new byte[0] : value.getBytes(StandardCharsets.ISO_8859_1);
        int n = Math.min(src.length, len);
        System.arraycopy(src, 0, buf, base + off, n);
        Arrays.fill(buf, base + off + n, base + off + len, SPACE);
    }

    protected final void fillX(int off, int len, byte value) {
        Arrays.fill(buf, base + off, base + off + len, value);
    }

    protected final boolean isAll(int off, int len, byte value) {
        for (int i = 0; i < len; i++) {
            if (buf[base + off + i] != value) {
                return false;
            }
        }
        return true;
    }

    protected final byte getByte(int off) {
        return buf[base + off];
    }

    protected final void setByte(int off, byte value) {
        buf[base + off] = value;
    }

    /** Unsigned zoned-decimal PIC 9(n). Throws if the field is not NUMERIC. */
    protected final long getNum(int off, int len) {
        long v = 0;
        for (int i = 0; i < len; i++) {
            byte b = buf[base + off + i];
            if (b < '0' || b > '9') {
                throw new NumberFormatException("Field at offset " + off + " is not NUMERIC: '" + getX(off, len) + "'");
            }
            v = v * 10 + (b - '0');
        }
        return v;
    }

    /** Numeric MOVE into PIC 9(n): sign dropped, high-order digits truncated. */
    protected final void setNum(int off, int len, long value) {
        long v = Math.abs(value);
        for (int i = len - 1; i >= 0; i--) {
            buf[base + off + i] = (byte) ('0' + (v % 10));
            v /= 10;
        }
    }

    protected final short getShort(int off) {
        return (short) (((buf[base + off] & 0xFF) << 8) | (buf[base + off + 1] & 0xFF));
    }

    protected final void setShort(int off, short value) {
        buf[base + off] = (byte) (value >> 8);
        buf[base + off + 1] = (byte) value;
    }

    /** True when every byte of the string is a space or every byte is LOW-VALUES (COBOL "= SPACES OR LOW-VALUES"). */
    public static boolean isSpacesOrLowValues(String s) {
        return s.chars().allMatch(c -> c == ' ') || s.chars().allMatch(c -> c == 0);
    }
}
