package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;

/**
 * One named field of a BMS symbolic map: L (length / cursor, COMP S9(4)), F/A (flag / attribute),
 * C (colour), P (programmed symbols), H (highlight), V (validation) and the I/O data.
 * After MOVE LOW-VALUES TO the map every byte is X'00'.
 */
public final class MapField {
    private final CcrdslaField field;
    private short length;
    private int attribute;
    private int color;
    private int ps;
    private int highlight;
    private int validation;
    private String data;

    MapField(CcrdslaField field) {
        this.field = field;
        this.data = CobolText.lowValues(field.length());
    }

    public CcrdslaField field() {
        return field;
    }

    /** xxxL. -1 requests symbolic cursor positioning. */
    public short length() {
        return length;
    }

    public void length(short v) {
        length = v;
    }

    /** xxxA (input map) / attribute byte (output map). */
    public int attribute() {
        return attribute;
    }

    public void attribute(int v) {
        attribute = v;
    }

    /** xxxC. */
    public int color() {
        return color;
    }

    public void color(int v) {
        color = v;
    }

    public int ps() {
        return ps;
    }

    public void ps(int v) {
        ps = v;
    }

    public int highlight() {
        return highlight;
    }

    public void highlight(int v) {
        highlight = v;
    }

    public int validation() {
        return validation;
    }

    public void validation(int v) {
        validation = v;
    }

    /** xxxO / xxxI data, always exactly the field length. */
    public String data() {
        return data;
    }

    /** MOVE alphanumeric TO xxxO (space-padded / truncated). */
    public void data(String v) {
        data = CobolText.pic(v, field.length());
    }

    /** MOVE LOW-VALUES TO xxxO. */
    public void lowValues() {
        data = CobolText.lowValues(field.length());
    }
}
