package com.carddemo.programs.cotrn00c.copybook;

/**
 * One BMS symbolic-map field: {@code xxxL} (COMP S9(4)), {@code xxxF}/{@code xxxA} (REDEFINES,
 * one byte), the COTRN0AO extended attributes {@code xxxC/P/H/V}, and the data {@code xxxI}/{@code xxxO}
 * (same storage, because COTRN0AO REDEFINES COTRN0AI).
 */
public final class MapField {

    private final String name;
    private final int size;
    private short length;
    private char attribute;
    private char color;
    private char ps;
    private char hilight;
    private char validn;
    private String data;

    public MapField(String name, int size) {
        this.name = name;
        this.size = size;
        setLowValues();
    }

    /** MOVE LOW-VALUES over the whole field entry. */
    public void setLowValues() {
        length = 0;
        attribute = CobolText.LOW_VALUE;
        color = CobolText.LOW_VALUE;
        ps = CobolText.LOW_VALUE;
        hilight = CobolText.LOW_VALUE;
        validn = CobolText.LOW_VALUE;
        data = CobolText.lowValues(size);
    }

    public String name() {
        return name;
    }

    public int size() {
        return size;
    }

    public short length() {
        return length;
    }

    /** MOVE n TO xxxL ({@code -1} requests symbolic cursor positioning). */
    public void setLength(int n) {
        length = (short) n;
    }

    public char attribute() {
        return attribute;
    }

    public void setAttribute(char a) {
        attribute = a;
    }

    public char color() {
        return color;
    }

    public char ps() {
        return ps;
    }

    public char hilight() {
        return hilight;
    }

    public char validn() {
        return validn;
    }

    public String data() {
        return data;
    }

    /** MOVE alphanumeric TO xxxI / xxxO. */
    public void set(String value) {
        data = CobolText.fit(value, size);
    }

    /** Raw assignment used for figurative constants already sized (e.g. LOW-VALUES). */
    public void setRaw(String value) {
        if (value.length() != size) {
            throw new IllegalArgumentException(name + " expects " + size + " bytes");
        }
        data = value;
    }
}
