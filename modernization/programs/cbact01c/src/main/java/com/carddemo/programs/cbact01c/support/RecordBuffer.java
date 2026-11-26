package com.carddemo.programs.cbact01c.support;

import java.math.BigDecimal;
import java.util.Arrays;

/** A fixed-length record area (FD record or WORKING-STORAGE group) addressed by {@link FieldDef}s. */
public class RecordBuffer {
    protected final byte[] data;

    protected RecordBuffer(int length, byte fill) {
        this.data = new byte[length];
        Arrays.fill(data, fill);
    }

    protected RecordBuffer(byte[] source) {
        this.data = source.clone();
    }

    public int length() {
        return data.length;
    }

    public byte[] toBytes() {
        return data.clone();
    }

    /** The whole record as DISPLAY would print it (raw bytes, one character per byte). */
    public String asText() {
        return RecordText.toText(data);
    }

    public byte[] raw(FieldDef f) {
        return Arrays.copyOfRange(data, f.offset(), f.end());
    }

    public String text(FieldDef f) {
        return RecordText.toText(raw(f));
    }

    public BigDecimal number(FieldDef f) {
        return switch (f.kind()) {
            case UNSIGNED_DISPLAY, SIGNED_DISPLAY -> ZonedDecimal.decode(text(f), f.scale());
            case PACKED -> PackedDecimal.decode(raw(f), f.scale());
            case ALPHANUMERIC -> throw new IllegalArgumentException(f.cobolName() + " is alphanumeric");
        };
    }

    /** Byte-for-byte MOVE between items of identical size and usage. */
    public void putRaw(FieldDef f, byte[] bytes) {
        if (bytes.length != f.length()) {
            throw new IllegalArgumentException(f.cobolName() + ": expected " + f.length() + " bytes, got " + bytes.length);
        }
        System.arraycopy(bytes, 0, data, f.offset(), f.length());
    }

    /** Alphanumeric MOVE: pad with spaces / truncate on the right. */
    public void putText(FieldDef f, String value) {
        putRaw(f, RecordText.toBytes(Pic.x(value, f.length())));
    }

    /** Numeric MOVE of a value (literal or computed) into a numeric item. */
    public void putNumber(FieldDef f, BigDecimal value) {
        switch (f.kind()) {
            case UNSIGNED_DISPLAY -> putRaw(f, RecordText.toBytes(ZonedDecimal.encode(value, f.intDigits(), f.scale(), false)));
            case SIGNED_DISPLAY -> putRaw(f, RecordText.toBytes(ZonedDecimal.encode(value, f.intDigits(), f.scale(), true)));
            case PACKED -> putRaw(f, PackedDecimal.encode(value, f.intDigits(), f.scale()));
            case ALPHANUMERIC -> throw new IllegalArgumentException(f.cobolName() + " is alphanumeric");
        }
    }

    /** INITIALIZE semantics for one item: numerics to zero, alphanumerics to spaces. */
    public void initialize(FieldDef f) {
        if (f.kind() == FieldDef.Kind.ALPHANUMERIC) {
            putText(f, "");
        } else {
            putNumber(f, BigDecimal.ZERO);
        }
    }
}
