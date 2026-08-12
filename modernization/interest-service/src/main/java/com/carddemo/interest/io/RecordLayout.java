package com.carddemo.interest.io;

import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * A copybook record layout: an ordered set of named, fixed-position fields over a fixed-length
 * EBCDIC record.
 *
 * <p>A layout is declared once from the copybook (see
 * {@link com.carddemo.interest.io.layout.CardDemoLayouts}) and then used to read and write
 * individual fields, so no offset arithmetic is scattered through the codebase. Field offsets are
 * derived from the declaration order, exactly as COBOL derives them from the {@code 01} level
 * structure.
 */
public final class RecordLayout {

    /** How a field's bytes encode its value. */
    public enum FieldKind {
        /** {@code PIC X(n)} — EBCDIC text. */
        ALPHANUMERIC,
        /** {@code PIC 9(n)} / {@code PIC S9(n)V9(m)} — one digit per byte. */
        ZONED_DECIMAL,
        /** {@code PIC S9(n)V9(m) COMP-3} — two digits per byte. */
        PACKED_DECIMAL
    }

    /** One field of a copybook layout. */
    public record FieldDefinition(String name, FieldKind kind, int offset, int byteLength,
                                  int digits, int scale, boolean signed) {
    }

    private final String name;
    private final Map<String, FieldDefinition> fields;
    private final int recordLength;

    private RecordLayout(String name, Map<String, FieldDefinition> fields, int recordLength) {
        this.name = name;
        this.fields = fields;
        this.recordLength = recordLength;
    }

    public static Builder named(String name) {
        return new Builder(name);
    }

    public String name() {
        return name;
    }

    /** Total record length in bytes, i.e. the copybook {@code RECLN}. */
    public int recordLength() {
        return recordLength;
    }

    public FieldDefinition field(String fieldName) {
        FieldDefinition definition = fields.get(fieldName);
        if (definition == null) {
            throw new IllegalArgumentException("No field '" + fieldName + "' in layout " + name);
        }
        return definition;
    }

    /** Reads an alphanumeric field, with trailing spaces removed. */
    public String text(byte[] record, String fieldName) {
        return rawText(record, fieldName).stripTrailing();
    }

    /** Reads an alphanumeric field, preserving trailing spaces. */
    public String rawText(byte[] record, String fieldName) {
        FieldDefinition definition = field(fieldName);
        return EbcdicText.toAscii(record, definition.offset(), definition.byteLength());
    }

    /** Reads a numeric field (zoned or packed) as an exact decimal. */
    public BigDecimal decimal(byte[] record, String fieldName) {
        FieldDefinition definition = field(fieldName);
        return switch (definition.kind()) {
            case ZONED_DECIMAL -> ZonedDecimalCodec.decode(
                    record, definition.offset(), definition.digits(), definition.scale());
            case PACKED_DECIMAL -> PackedDecimalCodec.decode(
                    record, definition.offset(), definition.digits(), definition.scale());
            case ALPHANUMERIC -> throw new IllegalArgumentException(
                    "Field '" + fieldName + "' of layout " + name + " is alphanumeric");
        };
    }

    /** Writes an alphanumeric field, space padded or truncated to the field width. */
    public void putText(byte[] record, String fieldName, String value) {
        FieldDefinition definition = field(fieldName);
        int width = definition.byteLength();
        String text = value.length() > width ? value.substring(0, width) : value;
        byte[] encoded = EbcdicText.toEbcdic(text + " ".repeat(width - text.length()));
        System.arraycopy(encoded, 0, record, definition.offset(), width);
    }

    /** Writes a numeric field, applying COBOL truncation to the declared PIC capacity. */
    public void putDecimal(byte[] record, String fieldName, BigDecimal value) {
        FieldDefinition definition = field(fieldName);
        switch (definition.kind()) {
            case ZONED_DECIMAL -> ZonedDecimalCodec.encode(record, definition.offset(),
                    definition.digits(), definition.scale(), definition.signed(), value);
            case PACKED_DECIMAL -> PackedDecimalCodec.encode(record, definition.offset(),
                    definition.digits(), definition.scale(), value);
            case ALPHANUMERIC -> throw new IllegalArgumentException(
                    "Field '" + fieldName + "' of layout " + name + " is alphanumeric");
        }
    }

    /** Allocates a blank (EBCDIC space filled) record of this layout's length. */
    public byte[] blankRecord() {
        return EbcdicText.blankRecord(recordLength);
    }

    /** Fluent declaration of a copybook layout. */
    public static final class Builder {
        private final String name;
        private final Map<String, FieldDefinition> fields = new LinkedHashMap<>();
        private int offset;

        private Builder(String name) {
            this.name = name;
        }

        /** Declares a {@code PIC X(length)} field. */
        public Builder alphanumeric(String fieldName, int length) {
            return add(new FieldDefinition(fieldName, FieldKind.ALPHANUMERIC, offset, length, 0, 0, false));
        }

        /** Declares an unsigned {@code PIC 9(digits)} field. */
        public Builder unsignedNumber(String fieldName, int digits) {
            return add(new FieldDefinition(fieldName, FieldKind.ZONED_DECIMAL, offset, digits, digits, 0, false));
        }

        /** Declares a signed {@code PIC S9(digits-scale)V9(scale)} display field. */
        public Builder signedDecimal(String fieldName, int digits, int scale) {
            return add(new FieldDefinition(fieldName, FieldKind.ZONED_DECIMAL, offset, digits, digits, scale, true));
        }

        /** Declares a signed {@code PIC S9(digits-scale)V9(scale) COMP-3} field. */
        public Builder packedDecimal(String fieldName, int digits, int scale) {
            int byteLength = PackedDecimalCodec.byteLength(digits);
            return add(new FieldDefinition(fieldName, FieldKind.PACKED_DECIMAL, offset, byteLength, digits, scale, true));
        }

        /** Declares unnamed filler of {@code length} bytes. */
        public Builder filler(int length) {
            offset += length;
            return this;
        }

        public RecordLayout build(int expectedRecordLength) {
            if (offset != expectedRecordLength) {
                throw new IllegalStateException("Layout " + name + " declares " + offset
                        + " bytes but the copybook RECLN is " + expectedRecordLength);
            }
            return new RecordLayout(name, Map.copyOf(fields), expectedRecordLength);
        }

        private Builder add(FieldDefinition definition) {
            if (fields.put(definition.name(), definition) != null) {
                throw new IllegalStateException("Duplicate field " + definition.name() + " in layout " + name);
            }
            offset += definition.byteLength();
            return this;
        }
    }
}
