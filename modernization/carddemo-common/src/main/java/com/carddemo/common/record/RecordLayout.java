package com.carddemo.common.record;

import com.carddemo.common.cobol.BinaryInteger;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.cobol.PackedDecimal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Fixed-length record layout of one copybook (e.g. CVTRA06Y, 350 bytes). Fields are declared in
 * copybook order and must tile the record exactly, so a wrong PICTURE fails at class load, not in a
 * golden comparison. One layout per copybook is declared with {@link #builder}.
 */
public class RecordLayout {

    private final String copybook;
    private final int length;
    private final List<Field> fields;
    private final Map<String, Field> byName;

    protected RecordLayout(String copybook, int length, List<Field> fields) {
        this.copybook = copybook;
        this.length = length;
        this.fields = List.copyOf(fields);
        Map<String, Field> map = new LinkedHashMap<>();
        int expected = 0;
        for (Field field : this.fields) {
            if (field.offset() != expected) {
                throw new IllegalArgumentException(copybook + ": " + field.name() + " starts at " + field.offset()
                        + ", expected " + expected);
            }
            expected = field.end();
            if (field.type() != FieldType.FILLER && map.putIfAbsent(field.name(), field) != null) {
                throw new IllegalArgumentException(copybook + ": duplicate field " + field.name());
            }
        }
        if (expected != length) {
            throw new IllegalArgumentException(copybook + ": fields cover " + expected + " bytes, record length is " + length);
        }
        this.byName = Collections.unmodifiableMap(map);
    }

    public static Builder builder(String copybook, int length) {
        return new Builder(copybook, length);
    }

    public String copybook() {
        return copybook;
    }

    public int length() {
        return length;
    }

    public List<Field> fields() {
        return fields;
    }

    public Field field(String name) {
        Field field = byName.get(name);
        if (field == null) {
            throw new IllegalArgumentException(copybook + " has no field " + name);
        }
        return field;
    }

    /** A record of this layout filled with blanks in {@code codePage}. */
    public CobolRecord blank(CodePage codePage) {
        byte[] bytes = new byte[length];
        java.util.Arrays.fill(bytes, codePage.space());
        return new CobolRecord(this, codePage, bytes);
    }

    /** Wraps an image read from a file; the length must equal the record length. */
    public CobolRecord wrap(byte[] bytes, CodePage codePage) {
        return new CobolRecord(this, codePage, bytes.clone());
    }

    /** Declares fields in copybook order; offsets are computed. */
    public static final class Builder {
        private final String copybook;
        private final int length;
        private final List<Field> fields = new ArrayList<>();
        private int offset;

        private Builder(String copybook, int length) {
            this.copybook = copybook;
            this.length = length;
        }

        /** {@code PIC X(n)}. */
        public Builder alphanumeric(String name, int n) {
            return add(new Field(name, offset, n, FieldType.ALPHANUMERIC, 0, 0, false));
        }

        /** {@code PIC 9(integerDigits)V9(scale)} or {@code PIC S9(...)} USAGE DISPLAY. */
        public Builder zoned(String name, int integerDigits, int scale, boolean signed) {
            return add(new Field(name, offset, integerDigits + scale, FieldType.ZONED, integerDigits, scale, signed));
        }

        /** {@code COMP-3}. */
        public Builder packed(String name, int integerDigits, int scale, boolean signed) {
            return add(new Field(name, offset, PackedDecimal.byteLength(integerDigits + scale), FieldType.PACKED,
                    integerDigits, scale, signed));
        }

        /** {@code COMP} / {@code BINARY}. */
        public Builder binary(String name, int integerDigits, int scale, boolean signed) {
            return add(new Field(name, offset, BinaryInteger.byteLength(integerDigits + scale), FieldType.BINARY,
                    integerDigits, scale, signed));
        }

        public Builder filler(int n) {
            return add(new Field("FILLER", offset, n, FieldType.FILLER, 0, 0, false));
        }

        public RecordLayout build() {
            return new RecordLayout(copybook, length, fields);
        }

        private Builder add(Field field) {
            fields.add(field);
            offset = field.end();
            return this;
        }
    }
}
