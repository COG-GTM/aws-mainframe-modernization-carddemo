package com.carddemo.runtime.cobol;

import java.math.BigDecimal;

/** PIC 9/S9 item of a given usage (DISPLAY, COMP-3, COMP). */
public final class NumericField extends CobolField {

    /** Storage usage of a numeric item. */
    public enum Usage {
        DISPLAY, COMP_3, COMP
    }

    private final Usage usage;
    private final int integerDigits;
    private final int scale;
    private final boolean signed;

    public NumericField(String name, int offset, Usage usage, int integerDigits, int scale, boolean signed) {
        super(name, offset, lengthOf(usage, integerDigits, scale));
        this.usage = usage;
        this.integerDigits = integerDigits;
        this.scale = scale;
        this.signed = signed;
    }

    public static int lengthOf(Usage usage, int integerDigits, int scale) {
        return switch (usage) {
            case DISPLAY -> ZonedDecimal.length(integerDigits, scale);
            case COMP_3 -> PackedDecimal.length(integerDigits, scale);
            case COMP -> BinaryInteger.length(integerDigits + scale);
        };
    }

    public Usage usage() {
        return usage;
    }

    public int integerDigits() {
        return integerDigits;
    }

    public int scale() {
        return scale;
    }

    public boolean signed() {
        return signed;
    }

    /** Value with the item's exact scale. Throws {@link NumberFormatException} on invalid data (S0C7). */
    public BigDecimal get(CobolRecord record) {
        return switch (usage) {
            case DISPLAY -> ZonedDecimal.decode(raw(record), scale);
            case COMP_3 -> PackedDecimal.decode(rawBytes(record), scale);
            case COMP -> BinaryInteger.decode(rawBytes(record), scale, signed);
        };
    }

    public long getLong(CobolRecord record) {
        return get(record).longValue();
    }

    /** MOVE / COMPUTE value TO item (truncating). */
    public void set(CobolRecord record, BigDecimal value) {
        store(record, value, false);
    }

    /** COMPUTE item ROUNDED = value. */
    public void setRounded(CobolRecord record, BigDecimal value) {
        store(record, value, true);
    }

    public void set(CobolRecord record, long value) {
        set(record, BigDecimal.valueOf(value));
    }

    private void store(CobolRecord record, BigDecimal value, boolean rounded) {
        BigDecimal v = CobolNumeric.store(value, integerDigits, scale, signed, rounded);
        switch (usage) {
            case DISPLAY -> record.setString(offset(), length(), ZonedDecimal.encode(v, integerDigits, scale, signed));
            case COMP_3 -> record.putBytes(offset(), PackedDecimal.encode(v, integerDigits, scale, signed));
            case COMP -> record.putBytes(offset(), BinaryInteger.encode(v, integerDigits, scale, signed));
        }
    }

    /** {@code IF item NUMERIC}. COMP items are always numeric. */
    public boolean isNumeric(CobolRecord record) {
        return switch (usage) {
            case DISPLAY -> ZonedDecimal.isNumeric(raw(record), signed);
            case COMP_3 -> PackedDecimal.isNumeric(rawBytes(record));
            case COMP -> true;
        };
    }

    @Override
    public Object describe(CobolRecord record) {
        if (isLowValues(record) && usage != Usage.COMP) {
            return null;
        }
        return isNumeric(record) ? get(record) : raw(record);
    }
}
