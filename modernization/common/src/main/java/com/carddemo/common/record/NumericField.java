package com.carddemo.common.record;

/** A {@code PIC [S]9(n)[V9(m)]} item with its usage. */
public final class NumericField extends Field {

    private final int digits;
    private final int scale;
    private final boolean signed;
    private final Usage usage;

    NumericField(String copybook, String name, String picture, int offset, int length, String view,
                 int digits, int scale, boolean signed, Usage usage) {
        super(copybook, name, picture, offset, length, view);
        this.digits = digits;
        this.scale = scale;
        this.signed = signed;
        this.usage = usage;
    }

    /** Total number of digits, integer and decimal. */
    public int digits() {
        return digits;
    }

    /** Number of digits after the implied decimal point ({@code V}). */
    public int scale() {
        return scale;
    }

    public boolean signed() {
        return signed;
    }

    public Usage usage() {
        return usage;
    }
}
