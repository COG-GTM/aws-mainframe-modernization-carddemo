package com.carddemo.legacyio;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Field values of one record. Alphanumeric and numeric-edited fields hold a {@link String} of exactly the field's
 * length (trailing spaces kept); numeric fields hold a {@link BigDecimal} with the picture's scale, or an
 * {@link InvalidValue} when the stored bytes are not a valid number. A numeric field that has not been set since it
 * was decoded is encoded back to its original bytes, so equivalent representations (a plain last digit in a signed
 * zoned field, sign nibble F in a signed packed field) survive a round trip unchanged.
 */
public final class Record {

    private final RecordLayout layout;
    private final Object[] values;
    private final byte[][] storedNumbers;

    Record(RecordLayout layout, Object[] values) {
        this(layout, values, new byte[values.length][]);
    }

    Record(RecordLayout layout, Object[] values, byte[][] storedNumbers) {
        this.layout = layout;
        this.values = values;
        this.storedNumbers = storedNumbers;
    }

    /** Bytes a numeric field was decoded from, while it has not been set since; null otherwise. */
    byte[] storedNumber(int index) {
        return storedNumbers[index];
    }

    /** A record initialized like COBOL {@code INITIALIZE}: spaces in text fields, zero in numeric fields. */
    public static Record initialized(RecordLayout layout) {
        List<FieldDefinition> fields = layout.fields();
        Object[] values = new Object[fields.size()];
        for (int i = 0; i < values.length; i++) {
            FieldDefinition field = fields.get(i);
            values[i] = field.isNumeric() ? BigDecimal.ZERO.setScale(field.picture().scale())
                    : " ".repeat(field.length());
        }
        return new Record(layout, values);
    }

    public RecordLayout layout() {
        return layout;
    }

    public Object get(String field) {
        return values[layout.indexOf(field)];
    }

    Object get(int index) {
        return values[index];
    }

    public String getString(String field) {
        Object value = get(field);
        if (value instanceof String text) {
            return text;
        }
        throw new IllegalStateException("field " + field + " is numeric; use getDecimal");
    }

    public BigDecimal getDecimal(String field) {
        Object value = get(field);
        if (value instanceof BigDecimal decimal) {
            return decimal;
        }
        if (value instanceof InvalidValue invalid) {
            throw new IllegalStateException("field " + field + " holds invalid data " + invalid);
        }
        throw new IllegalStateException("field " + field + " is not numeric; use getString");
    }

    public long getLong(String field) {
        return getDecimal(field).longValueExact();
    }

    public boolean isValid(String field) {
        return !(get(field) instanceof InvalidValue);
    }

    /** Names of the numeric fields whose stored bytes are not a valid value, in layout order. */
    public List<String> invalidFields() {
        List<String> names = new ArrayList<>();
        for (int i = 0; i < values.length; i++) {
            if (values[i] instanceof InvalidValue) {
                names.add(layout.fields().get(i).name());
            }
        }
        return names;
    }

    /**
     * Sets a field. Text is padded with spaces to the field length and must not be longer; numbers must fit the
     * picture's digits and scale exactly (no rounding or truncation).
     */
    public Record set(String field, Object value) {
        return set(layout.indexOf(field), value);
    }

    Record set(int index, Object value) {
        values[index] = normalize(layout.fields().get(index), value);
        storedNumbers[index] = null;
        return this;
    }

    /** Named field values in layout order, FILLER excluded. */
    public Map<String, Object> toMap() {
        Map<String, Object> map = new LinkedHashMap<>();
        for (int i = 0; i < values.length; i++) {
            FieldDefinition field = layout.fields().get(i);
            if (!field.isFiller()) {
                map.put(field.name(), values[i]);
            }
        }
        return map;
    }

    private static Object normalize(FieldDefinition field, Object value) {
        if (value instanceof InvalidValue invalid) {
            if (invalid.raw().length != field.length()) {
                throw new IllegalArgumentException("invalid value for " + field.name() + " has the wrong length");
            }
            return invalid;
        }
        if (!field.isNumeric()) {
            if (!(value instanceof String text)) {
                throw new IllegalArgumentException("field " + field.name() + " takes text");
            }
            if (text.length() > field.length()) {
                throw new IllegalArgumentException(
                        "value of %d characters does not fit %s (%d)".formatted(text.length(), field.name(), field.length()));
            }
            return text + " ".repeat(field.length() - text.length());
        }
        BigDecimal decimal = switch (value) {
            case BigDecimal d -> d;
            case BigInteger b -> new BigDecimal(b);
            case Long l -> BigDecimal.valueOf(l);
            case Integer n -> BigDecimal.valueOf(n);
            case String s -> new BigDecimal(s);
            default -> throw new IllegalArgumentException("field " + field.name() + " takes a number");
        };
        Picture picture = field.picture();
        try {
            decimal = decimal.setScale(picture.scale());
        } catch (ArithmeticException e) {
            throw new IllegalArgumentException(decimal + " has more than " + picture.scale() + " decimals for " + field.name());
        }
        if (decimal.signum() < 0 && !picture.signed()) {
            throw new IllegalArgumentException("unsigned field " + field.name() + " cannot hold " + decimal);
        }
        if (decimal.precision() - decimal.scale() > picture.digits() - picture.scale() && decimal.signum() != 0) {
            throw new IllegalArgumentException(decimal + " does not fit " + field.name());
        }
        return decimal;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof Record that && layout.fields().equals(that.layout.fields())
                && Arrays.equals(values, that.values);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(values);
    }

    @Override
    public String toString() {
        return layout.name() + toMap();
    }
}
