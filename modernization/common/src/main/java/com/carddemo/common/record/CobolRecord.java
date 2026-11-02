package com.carddemo.common.record;

import com.carddemo.common.cobol.CobolValues;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * One fixed-width record held as its bytes, read and written through the typed fields of its
 * {@link RecordLayout}, the way COBOL addresses a record area. Keeping the bytes means FILLER,
 * REDEFINES and values the program never touches survive a read/write round trip unchanged.
 */
public final class CobolRecord {

    private final RecordLayout layout;
    private final CodePage codePage;
    private final byte[] data;

    private CobolRecord(RecordLayout layout, CodePage codePage, byte[] data) {
        this.layout = layout;
        this.codePage = codePage;
        this.data = data;
    }

    /** Wraps a copy of {@code bytes}, which must be exactly one record long. */
    public static CobolRecord of(RecordLayout layout, CodePage codePage, byte[] bytes) {
        if (bytes.length != layout.length()) {
            throw new IllegalArgumentException(layout.copybook() + " record is " + layout.length()
                    + " bytes, got " + bytes.length);
        }
        return new CobolRecord(layout, codePage, bytes.clone());
    }

    /** A record as {@code INITIALIZE} leaves it: alphanumeric fields (FILLER too) spaces, numeric fields zero. */
    public static CobolRecord initialized(RecordLayout layout, CodePage codePage) {
        CobolRecord record = new CobolRecord(layout, codePage, new byte[layout.length()]);
        for (Field field : layout.fields()) {
            if (field instanceof AlphanumericField alpha) {
                record.set(alpha, "");
            } else {
                record.set((NumericField) field, BigDecimal.ZERO);
            }
        }
        return record;
    }

    public RecordLayout layout() {
        return layout;
    }

    public CodePage codePage() {
        return codePage;
    }

    /** The record bytes (a copy). */
    public byte[] toBytes() {
        return data.clone();
    }

    /** The field's text including its blank padding. */
    public String get(AlphanumericField field) {
        check(field);
        return codePage.decodeText(data, field.offset(), field.length());
    }

    /** The field's text without blank padding; null when blank (see {@link CobolValues#trim}). */
    public String getTrimmed(AlphanumericField field) {
        return CobolValues.trim(get(field));
    }

    public BigDecimal get(NumericField field) {
        check(field);
        try {
            BigDecimal value = switch (field.usage()) {
                case DISPLAY -> codePage.decodeZoned(data, field.offset(), field);
                case PACKED -> {
                    BigDecimal packed = CobolValues.decodePacked(slice(field), field.scale());
                    if (!field.signed() && packed.signum() < 0) {
                        throw new IllegalArgumentException("negative sign in unsigned field");
                    }
                    yield packed;
                }
                case BINARY -> CobolValues.decodeBinary(slice(field), field.signed(), field.scale());
            };
            if (value.unscaledValue().abs().toString().length() > field.digits()) {
                throw new IllegalArgumentException(value + " exceeds the picture's " + field.digits() + " digits");
            }
            return value;
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException(describe(field) + ": " + ex.getMessage(), ex);
        }
    }

    /** An integer field ({@code V} absent) as a long. */
    public long getLong(NumericField field) {
        if (field.scale() != 0) {
            throw new IllegalArgumentException(describe(field) + " has decimals");
        }
        return get(field).longValueExact();
    }

    public int getInt(NumericField field) {
        return Math.toIntExact(getLong(field));
    }

    /**
     * COBOL {@code IF field IS NUMERIC}: the bytes decode as a valid value of the field's picture, including
     * binary and packed values whose storage could hold more digits than the picture allows.
     */
    public boolean isNumeric(NumericField field) {
        try {
            get(field);
            return true;
        } catch (IllegalArgumentException ex) {
            return false;
        }
    }

    /** {@code MOVE value TO field}: left-justified, blank-padded, cut on the right; null moves spaces. */
    public CobolRecord set(AlphanumericField field, String value) {
        check(field);
        byte[] text = codePage.encodeText(CobolValues.padRight(value, field.length()));
        System.arraycopy(text, 0, data, field.offset(), field.length());
        return this;
    }

    /**
     * Stores {@code value}, dropping excess decimals toward zero (conventions.md §2). A value whose
     * integer part does not fit the picture, or a negative value in an unsigned field, is rejected
     * instead of being silently truncated.
     */
    public CobolRecord set(NumericField field, BigDecimal value) {
        check(field);
        BigDecimal truncated = value.setScale(field.scale(), RoundingMode.DOWN);
        if (truncated.unscaledValue().abs().toString().length() > field.digits()) {
            throw new IllegalArgumentException(value + " does not fit " + describe(field));
        }
        if (truncated.signum() < 0 && !field.signed()) {
            throw new IllegalArgumentException(value + " is negative, " + describe(field) + " is unsigned");
        }
        byte[] encoded = switch (field.usage()) {
            case DISPLAY -> {
                codePage.encodeZoned(truncated, field, data, field.offset());
                yield null;
            }
            case PACKED -> CobolValues.encodePacked(truncated, field.scale(), field.length(), field.signed());
            case BINARY -> CobolValues.encodeBinary(truncated, field.scale(), field.length(), field.signed());
        };
        if (encoded != null) {
            System.arraycopy(encoded, 0, data, field.offset(), field.length());
        }
        return this;
    }

    public CobolRecord set(NumericField field, long value) {
        return set(field, BigDecimal.valueOf(value));
    }

    /** Values of the primary storage map in copybook order (text untrimmed, numbers as BigDecimal). */
    public Map<Field, Object> values() {
        return values(layout.fields());
    }

    /** Values of the given fields, e.g. the header plus one REDEFINES view. */
    public Map<Field, Object> values(List<? extends Field> fields) {
        Map<Field, Object> values = new LinkedHashMap<>();
        for (Field field : fields) {
            values.put(field, field instanceof AlphanumericField alpha ? get(alpha) : get((NumericField) field));
        }
        return Collections.unmodifiableMap(values);
    }

    /**
     * The same record in another code page, translating the primary storage map field by field. Layouts
     * with REDEFINES views need {@link #withCodePage(CodePage, List)}: which bytes are text depends on the view.
     */
    public CobolRecord withCodePage(CodePage target) {
        if (layout.hasRedefines()) {
            throw new IllegalStateException(layout + " has REDEFINES views; pass the storage map of the view in use");
        }
        return withCodePage(target, layout.fields());
    }

    /**
     * The same record in another code page, translating {@code storage} field by field; use the header
     * plus the matching view for a record with REDEFINES. Bytes outside {@code storage} are copied as is.
     */
    public CobolRecord withCodePage(CodePage target, List<? extends Field> storage) {
        CobolRecord converted = new CobolRecord(layout, target, data.clone());
        if (target == codePage) {
            return converted;
        }
        for (Field field : storage) {
            if (field instanceof AlphanumericField alpha) {
                converted.set(alpha, get(alpha));
            } else {
                NumericField numeric = (NumericField) field;
                if (numeric.usage() == Usage.DISPLAY) {
                    converted.set(numeric, get(numeric));
                }
            }
        }
        return converted;
    }

    private byte[] slice(Field field) {
        return Arrays.copyOfRange(data, field.offset(), field.offset() + field.length());
    }

    private void check(Field field) {
        if (!field.copybook().equals(layout.copybook())) {
            throw new IllegalArgumentException(field + " is not a field of " + layout);
        }
    }

    private String describe(Field field) {
        return field.copybook() + " " + field.name() + " (PIC " + field.picture() + ", offset " + field.offset() + ")";
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof CobolRecord that
                && layout == that.layout
                && codePage == that.codePage
                && Arrays.equals(data, that.data);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(data);
    }

    @Override
    public String toString() {
        return layout.copybook() + "[" + codePage + "] " + codePage.decodeText(data, 0, data.length);
    }
}
