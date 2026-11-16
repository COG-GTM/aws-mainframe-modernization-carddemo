package com.carddemo.legacyio;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.HashMap;
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
    private final Encoding sourceEncoding;
    private final Map<String, byte[]> sourceBytes;
    private LineFraming framing;

    private Record(RecordLayout layout, Map<String, Object> values, Encoding sourceEncoding,
            Map<String, byte[]> sourceBytes) {
        this.layout = layout;
        this.values = values;
        this.sourceEncoding = sourceEncoding;
        this.sourceBytes = sourceBytes;
    }

    /** A new record with alphanumeric fields set to spaces and numeric fields to zero. */
    public static Record blank(RecordLayout layout) {
        Map<String, Object> values = new LinkedHashMap<>();
        for (FieldDefinition field : layout.fields()) {
            values.put(field.name(), field.isNumeric()
                    ? BigDecimal.ZERO.setScale(field.picture().scale())
                    : " ".repeat(field.length()));
        }
        return new Record(layout, values, null, new HashMap<>());
    }

    /**
     * A decoded record. {@code sourceBytes} holds the stored bytes of numeric fields, written back unchanged in the
     * same encoding until the field is set, so non-preferred signs (unsigned zone, {@code F}/{@code A}/{@code E}/
     * {@code B} nibbles, negative zero) survive a round trip.
     */
    static Record decoded(RecordLayout layout, Map<String, Object> values, Encoding encoding,
            Map<String, byte[]> sourceBytes) {
        return new Record(layout, values, encoding, sourceBytes);
    }

    /** The bytes a numeric field was decoded from, if it has not been set since and the encoding matches. */
    byte[] sourceBytes(String field, Encoding encoding) {
        return encoding == sourceEncoding ? sourceBytes.get(field) : null;
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
        if (value instanceof InvalidFieldData invalid) {
            throw new InvalidFieldDataException(layout.name() + "." + field + " holds invalid numeric data " + invalid);
        }
        BigDecimal decimal = FieldCodec.toDecimal(value);
        if (decimal == null) {
            throw new IllegalArgumentException(field + " is not numeric");
        }
        try {
            return decimal.setScale(layout.field(field).picture().scale(), RoundingMode.UNNECESSARY);
        } catch (ArithmeticException e) {
            return decimal;
        }
    }

    public long getLong(String field) {
        return getDecimal(field).longValueExact();
    }

    /** Sets a field; the value is checked against the picture when the record is encoded. */
    public Record set(String field, Object value) {
        layout.field(field);
        values.put(field, Objects.requireNonNull(value, field));
        sourceBytes.remove(field);
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
