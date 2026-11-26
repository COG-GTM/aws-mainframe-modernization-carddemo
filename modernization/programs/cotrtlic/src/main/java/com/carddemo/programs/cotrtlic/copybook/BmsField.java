package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;

/**
 * One BMS map field as laid out in the symbolic map: input view {@code xxxL / xxxF(A) / xxxI} and
 * output view {@code xxxC / xxxP / xxxH / xxxV / xxxO} share the same storage, as in COTRTLI.cpy.
 */
public final class BmsField {

    private final int size;
    /** xxxL PIC S9(4) COMP: received length; -1 positions the cursor on SEND ... CURSOR. */
    public int length;
    /** xxxF / xxxA PIC X: input flag byte / output attribute byte. */
    public char attribute;
    /** xxxC PIC X: colour. */
    public char color;
    /** xxxP PIC X: programmed symbols. */
    public char ps;
    /** xxxH PIC X: highlighting. */
    public char highlight;
    /** xxxV PIC X: validation. */
    public char validation;
    /** xxxI / xxxO PIC X(size). */
    private String data;

    public BmsField(int size) {
        this.size = size;
        clear();
    }

    /** MOVE LOW-VALUES over the whole field. */
    public void clear() {
        length = 0;
        attribute = Cob.LOW_VALUE;
        color = Cob.LOW_VALUE;
        ps = Cob.LOW_VALUE;
        highlight = Cob.LOW_VALUE;
        validation = Cob.LOW_VALUE;
        data = Cob.lowValues(size);
    }

    public int size() {
        return size;
    }

    public String data() {
        return data;
    }

    /** MOVE x TO xxxO (alphanumeric move: truncate/pad with spaces). */
    public void setData(String value) {
        data = Cob.move(value, size);
    }

    /** Sets the raw field bytes (used for LOW-VALUES moves and terminal input). */
    public void setRaw(String value) {
        data = value.length() == size ? value : Cob.move(value, size);
    }

    /** Data as typed on the terminal: BMS pads modified alphanumeric input on the right with blanks. */
    public void receive(String typed) {
        if (typed == null) {
            length = 0;
            data = Cob.lowValues(size);
        } else {
            String t = typed.length() > size ? typed.substring(0, size) : typed;
            length = t.length();
            data = Cob.move(t, size);
        }
    }

    /** Displayed text with LOW-VALUES rendered as blanks. */
    public String displayed() {
        return data.replace(Cob.LOW_VALUE, ' ');
    }

    public BmsField copy() {
        BmsField f = new BmsField(size);
        f.length = length;
        f.attribute = attribute;
        f.color = color;
        f.ps = ps;
        f.highlight = highlight;
        f.validation = validation;
        f.data = data;
        return f;
    }
}
