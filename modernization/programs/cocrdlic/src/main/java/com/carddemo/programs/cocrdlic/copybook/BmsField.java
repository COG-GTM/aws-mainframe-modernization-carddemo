package com.carddemo.programs.cocrdlic.copybook;

/**
 * One named field of a BMS symbolic map: the L (length / cursor), F/A (attribute), C (colour) and I/O data parts.
 * Data is held at the field's fixed length; LOW-VALUES means "not set / not transmitted".
 */
public final class BmsField {
    public static final short CURSOR = -1;

    private final String name;
    private final int length;
    private short l;
    private char attribute;
    private char color;
    private String data;

    public BmsField(String name, int length) {
        this.name = name;
        this.length = length;
        clear();
    }

    /** MOVE LOW-VALUES to the whole field group. */
    public void clear() {
        l = 0;
        attribute = Pic.LOW_VALUE;
        color = Pic.LOW_VALUE;
        data = Pic.lowValues(length);
    }

    public String name() { return name; }
    public int length() { return length; }
    public short l() { return l; }
    public void setL(short l) { this.l = l; }
    public boolean hasCursor() { return l == CURSOR; }
    public char attribute() { return attribute; }
    public void setAttribute(char attribute) { this.attribute = attribute; }
    public char color() { return color; }
    public void setColor(char color) { this.color = color; }
    public String data() { return data; }

    /** MOVE alphanumeric TO xxxO: left-justified, truncated / space padded to the field length. */
    public void setData(String value) {
        this.data = value == null ? Pic.lowValues(length) : Pic.x(value, length);
    }

    /** Data with LOW-VALUES rendered as spaces and trailing spaces removed (what a 3270 operator sees). */
    public String text() {
        return data.replace(Pic.LOW_VALUE, ' ').stripTrailing();
    }

    @Override
    public String toString() {
        return name + "='" + text() + "'";
    }
}
