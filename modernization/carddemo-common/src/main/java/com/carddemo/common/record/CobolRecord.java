package com.carddemo.common.record;

import com.carddemo.common.cobol.BinaryInteger;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.cobol.PackedDecimal;
import com.carddemo.common.cobol.ZonedDecimal;

import java.math.BigDecimal;
import java.util.Arrays;

/**
 * One fixed-length record image with typed access through its {@link RecordLayout}. The bytes stay
 * in the file's {@link CodePage}; text and zoned fields are converted on access, packed and binary
 * fields are never code-page converted. Unchanged bytes (FILLER, LOW-VALUES) survive a read/write
 * round trip unchanged.
 */
public final class CobolRecord {

    private final RecordLayout layout;
    private final CodePage codePage;
    private final byte[] bytes;

    CobolRecord(RecordLayout layout, CodePage codePage, byte[] bytes) {
        if (bytes.length != layout.length()) {
            throw new IllegalArgumentException(layout.copybook() + " record is " + layout.length() + " bytes, got " + bytes.length);
        }
        this.layout = layout;
        this.codePage = codePage;
        this.bytes = bytes;
    }

    public RecordLayout layout() {
        return layout;
    }

    public CodePage codePage() {
        return codePage;
    }

    /** A copy of the record image. */
    public byte[] bytes() {
        return bytes.clone();
    }

    public byte[] rawField(String name) {
        Field field = layout.field(name);
        return Arrays.copyOfRange(bytes, field.offset(), field.end());
    }

    /** The {@code PIC X} value with its padding, exactly as stored. */
    public String getString(String name) {
        Field field = layout.field(name);
        return codePage.decode(bytes, field.offset(), field.length());
    }

    /** The {@code PIC X} value trimmed; an all-blank field is {@code null} (CORE type mapping). */
    public String getTrimmed(String name) {
        String trimmed = getString(name).trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    /** {@code MOVE value TO field}: left-justified, blank-padded, right-truncated. */
    public CobolRecord setString(String name, String value) {
        Field field = layout.field(name);
        byte[] encoded = codePage.encode(value == null ? "" : value);
        Arrays.fill(bytes, field.offset(), field.end(), codePage.space());
        System.arraycopy(encoded, 0, bytes, field.offset(), Math.min(encoded.length, field.length()));
        return this;
    }

    public BigDecimal getDecimal(String name) {
        Field field = layout.field(name);
        return switch (field.type()) {
            case ZONED -> ZonedDecimal.decode(codePage.decode(bytes, field.offset(), field.length()), field.scale());
            case PACKED -> PackedDecimal.decode(bytes, field.offset(), field.length(), field.scale());
            case BINARY -> BinaryInteger.decode(bytes, field.offset(), field.length(),
                    field.integerDigits() + field.scale(), field.scale(), field.signed());
            default -> throw new IllegalArgumentException(name + " is not numeric");
        };
    }

    /** Unsigned integer field ({@code PIC 9(n)}, e.g. an account id) as a {@code long}. */
    public long getLong(String name) {
        return getDecimal(name).longValueExact();
    }

    /** Stores a value that must fit the field's picture (see {@code CobolDecimal.store} for COBOL truncation). */
    public CobolRecord setDecimal(String name, BigDecimal value) {
        Field field = layout.field(name);
        byte[] encoded = switch (field.type()) {
            case ZONED -> codePage.encode(ZonedDecimal.encode(value, field.integerDigits(), field.scale(), field.signed()));
            case PACKED -> PackedDecimal.encode(value, field.integerDigits(), field.scale(), field.signed());
            case BINARY -> BinaryInteger.encode(value, field.integerDigits(), field.scale(), field.signed());
            default -> throw new IllegalArgumentException(name + " is not numeric");
        };
        System.arraycopy(encoded, 0, bytes, field.offset(), field.length());
        return this;
    }

    public CobolRecord setLong(String name, long value) {
        return setDecimal(name, BigDecimal.valueOf(value));
    }
}
