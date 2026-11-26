package com.carddemo.programs.cousr00c.copybook;

import com.carddemo.programs.cousr00c.Cobol;

/**
 * One named BMS field of a symbolic map. The input (xxxI) and output (xxxO) views share the
 * same storage in COBOL (COUSR0AO REDEFINES COUSR0AI), so a single data value backs both.
 */
public final class BmsField {

    private final String name;
    private final int length;
    /** xxxL COMP PIC S9(4): received length, or -1 for symbolic cursor positioning. */
    private short l;
    /** xxxF / xxxA PIC X: flag byte (input) / attribute byte (output). */
    private char attribute;
    /** xxxC / xxxP / xxxH / xxxV PIC X: colour, PS, highlight, validation. */
    private char color;
    private char ps;
    private char highlight;
    private char validation;
    private String data;

    public BmsField(String name, int length) {
        this.name = name;
        this.length = length;
        lowValues();
    }

    /** MOVE LOW-VALUES TO the whole field group (length, attributes and data). */
    public void lowValues() {
        l = 0;
        attribute = Cobol.LOW_VALUE;
        color = Cobol.LOW_VALUE;
        ps = Cobol.LOW_VALUE;
        highlight = Cobol.LOW_VALUE;
        validation = Cobol.LOW_VALUE;
        data = Cobol.lowValues(length);
    }

    /** MOVE value TO xxxI / xxxO (alphanumeric move). */
    public void move(String value) {
        data = Cobol.alnum(value, length);
    }

    /** Field as delivered by RECEIVE MAP: null means not transmitted (low-values, L = 0). */
    public void receive(String value) {
        if (value == null) {
            data = Cobol.lowValues(length);
            l = 0;
        } else {
            data = Cobol.alnum(value, length);
            l = (short) Math.min(value.length(), length);
        }
    }

    public String name() {
        return name;
    }

    public int length() {
        return length;
    }

    public String data() {
        return data;
    }

    public short getL() {
        return l;
    }

    public void setL(int value) {
        this.l = (short) value;
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

    public void setPs(char ps) {
        this.ps = ps;
    }

    public char getHighlight() {
        return highlight;
    }

    public void setHighlight(char highlight) {
        this.highlight = highlight;
    }

    public char getValidation() {
        return validation;
    }

    public void setValidation(char validation) {
        this.validation = validation;
    }
}
