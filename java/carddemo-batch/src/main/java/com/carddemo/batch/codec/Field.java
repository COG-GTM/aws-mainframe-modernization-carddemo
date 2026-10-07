package com.carddemo.batch.codec;

import java.util.Collections;
import java.util.List;

/**
 * One elementary or group item of a copybook layout: its name, byte offset inside the record,
 * byte length, usage and (for numerics) digit count, implied decimals and sign.
 * A group with {@code occurs > 1} is a fixed-size COBOL OCCURS table; {@link #occurrence(int)}
 * returns the same field shifted to the n-th occurrence (0-based).
 */
public final class Field {

    public enum Usage {
        /** PIC X: fixed-width text, trailing spaces preserved. */
        TEXT,
        /** PIC 9 / PIC S9 USAGE DISPLAY: zoned decimal. */
        ZONED,
        /** USAGE COMP-3: packed decimal. */
        PACKED,
        /** A group item (has children). */
        GROUP,
        /** FILLER: never decoded, kept as bytes. */
        FILLER
    }

    private final String name;
    private final int offset;
    private final int length;
    private final Usage usage;
    private final int digits;
    private final int scale;
    private final boolean signed;
    private final int occurs;
    private final List<Field> children;

    Field(String name, int offset, int length, Usage usage, int digits, int scale, boolean signed,
          int occurs, List<Field> children) {
        this.name = name;
        this.offset = offset;
        this.length = length;
        this.usage = usage;
        this.digits = digits;
        this.scale = scale;
        this.signed = signed;
        this.occurs = occurs;
        this.children = children == null ? Collections.emptyList() : List.copyOf(children);
    }

    public String name() {
        return name;
    }

    public int offset() {
        return offset;
    }

    /** Byte length of one occurrence. */
    public int length() {
        return length;
    }

    public Usage usage() {
        return usage;
    }

    public int digits() {
        return digits;
    }

    public int scale() {
        return scale;
    }

    public boolean signed() {
        return signed;
    }

    public int occurs() {
        return occurs;
    }

    public List<Field> children() {
        return children;
    }

    /** This field relocated to occurrence {@code index} (0-based) of its OCCURS table. */
    public Field occurrence(int index) {
        if (index < 0 || index >= occurs) {
            throw new IndexOutOfBoundsException(name + " has " + occurs + " occurrences, asked for " + index);
        }
        return relocated(index * length, 1);
    }

    /** Child of this group named {@code childName}, with its offset already absolute. */
    public Field child(String childName) {
        for (Field c : children) {
            if (c.name.equals(childName)) {
                return c;
            }
        }
        throw new IllegalArgumentException("no field " + childName + " in group " + name);
    }

    /** Same field (and descendants, keeping their own OCCURS counts) moved {@code delta} bytes. */
    Field shifted(int delta) {
        return relocated(delta, occurs);
    }

    private Field relocated(int delta, int occursOfResult) {
        List<Field> kids = children.stream().map(c -> c.shifted(delta)).toList();
        return new Field(name, offset + delta, length, usage, digits, scale, signed, occursOfResult, kids);
    }

    @Override
    public String toString() {
        return name + "@" + offset + "+" + length + (occurs > 1 ? " OCCURS " + occurs : "");
    }
}
