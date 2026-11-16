package com.carddemo.legacyio;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Field values of one record, keyed by the field names of its {@link RecordLayout}. Alphanumeric fields hold
 * {@link String}s, numeric fields {@link BigDecimal}s with the picture's scale (or {@link InvalidFieldData}).
 */
public final class Record {

    private final RecordLayout layout;
    private final Map<String, Object> values;
    private LineFraming framing;

    private Record(RecordLayout layout, Map<String, Object> values, LineFraming framing) {
        this.layout = layout;
        this.values = values;
        this.framing = framing;
    }

    /** A new record with alphanumeric fields set to spaces and numeric fields to zero. */
    public static Record blank(RecordLayout layout) {
        Map<String, Object> values = new LinkedHashMap<>();
        for (FieldDefinition field : layout.fields()) {
            values.put(field.name(), field.isNumeric()
                    ? BigDecimal.ZERO.setScale(field.picture().scale())
                    : " ".repeat(field.length()));
        }
        return new Record(layout, values, null);
    }

    static Record decoded(RecordLayout layout, Map<String, Object> values) {
        return new Record(layout, values, null);
    }

    public RecordLayout layout() {
        return layout;
    }

    /** The decoded value of a field. */
    public Object get(String field) {
        layout.field(field);
        return values.get(field);
    }

    public String getString(String field) {
        Object value = get(field);
        if (value instanceof String s) {
            return s;
        }
        throw new IllegalArgumentException(field + " is not alphanumeric");
    }

    /** A numeric field's value; throws {@link InvalidFieldDataException} when its bytes are not a valid number. */
    public BigDecimal getDecimal(String field) {
        Object value = get(field);
        if (value instanceof BigDecimal d) {
            return d;
        }
        if (value instanceof InvalidFieldData invalid) {
            throw new InvalidFieldDataException(layout.name() + "." + field + " holds invalid numeric data " + invalid);
        }
        throw new IllegalArgumentException(field + " is not numeric");
    }

    public long getLong(String field) {
        return getDecimal(field).longValueExact();
    }

    /** Sets a field; the value is checked against the picture when the record is encoded. */
    public Record set(String field, Object value) {
        layout.field(field);
        values.put(field, Objects.requireNonNull(value, field));
        return this;
    }

    /** Fields whose bytes did not form a valid number for their picture. */
    public List<String> invalidFields() {
        List<String> invalid = new ArrayList<>();
        values.forEach((name, value) -> {
            if (value instanceof InvalidFieldData) {
                invalid.add(name);
            }
        });
        return invalid;
    }

    /** All values in record order. */
    public Map<String, Object> values() {
        return Collections.unmodifiableMap(values);
    }

    /** How the record was stored in a line-sequential file, or {@code null} when it was not read from one. */
    public LineFraming framing() {
        return framing;
    }

    public Record framing(LineFraming lineFraming) {
        this.framing = lineFraming;
        return this;
    }

    @Override
    public String toString() {
        return layout.name() + values;
    }
}
