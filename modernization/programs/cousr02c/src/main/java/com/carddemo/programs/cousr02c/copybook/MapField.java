package com.carddemo.programs.cousr02c.copybook;

/**
 * One named field of a BMS symbolic map: {@code xxxL} (S9(4) COMP length / cursor flag), {@code xxxF}/{@code xxxA}
 * (flag / attribute byte), the EXTATT=YES bytes {@code xxxC} (colour), {@code xxxP} (PS), {@code xxxH} (highlight),
 * {@code xxxV} (validation) and the data, which is shared by {@code xxxI} and {@code xxxO} because the output map
 * REDEFINES the input map.
 */
public final class MapField {

    /** {@code MOVE -1 TO xxxL}: symbolic cursor positioning when the map is sent with CURSOR. */
    public static final short CURSOR = -1;

    private final String name;
    private final int size;
    private short length;
    private char attribute;
    private char color;
    private char ps;
    private char hilight;
    private char validn;
    private String data;

    MapField(String name, int size) {
        this.name = name;
        this.size = size;
        lowValues();
    }

    /** MOVE LOW-VALUES over the field's slice of the map. */
    void lowValues() {
        length = 0;
        attribute = Pic.LOW_VALUE;
        color = Pic.LOW_VALUE;
        ps = Pic.LOW_VALUE;
        hilight = Pic.LOW_VALUE;
        validn = Pic.LOW_VALUE;
        data = Pic.lowValues(size);
    }

    public String name() {
        return name;
    }

    public int size() {
        return size;
    }

    public short getLength() {
        return length;
    }

    public void setLength(short length) {
        this.length = length;
    }

    public void setCursor() {
        this.length = CURSOR;
    }

    public char getAttribute() {
        return attribute;
    }

    public void setAttribute(char attribute) {
        this.attribute = attribute;
    }

    public char getColor() {
        return color;
    }

    public void setColor(char color) {
        this.color = color;
    }

    public char getPs() {
        return ps;
    }

    public char getHilight() {
        return hilight;
    }

    public char getValidn() {
        return validn;
    }

    /** {@code xxxI} / {@code xxxO}. */
    public String getData() {
        return data;
    }

    /** MOVE alphanumeric into {@code xxxI} / {@code xxxO} (truncate / space-pad). */
    public void setData(String value) {
        this.data = Pic.x(value, size);
    }

    /** Raw byte image assignment (used for LOW-VALUES and RECEIVE MAP). */
    public void setRawData(String value) {
        if (value.length() != size) {
            throw new IllegalArgumentException(name + " expects " + size + " bytes");
        }
        this.data = value;
    }
}
