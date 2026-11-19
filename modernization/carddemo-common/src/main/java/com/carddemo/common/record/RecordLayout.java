package com.carddemo.common.record;

import com.carddemo.common.cobol.ZonedDecimal;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Fixed-width layout of one copybook record. Offsets are derived from the declared field order, and the total must equal
 * the declared record length. FILLER is not exposed; it is emitted as its fill character (D-020, D-036).
 */
public final class RecordLayout {

    public enum Kind { ALPHA, UNSIGNED, SIGNED, FILLER }

    public record Field(String name, Kind kind, int offset, int length, int scale, char fill) {
        public int end() {
            return offset + length;
        }
    }

    private final String name;
    private final int length;
    private final List<Field> fields;
    private final Map<String, Field> byName;

    private RecordLayout(String name, int length, List<Field> fields) {
        this.name = name;
        this.length = length;
        this.fields = List.copyOf(fields);
        Map<String, Field> map = new LinkedHashMap<>();
        for (Field f : fields) {
            if (f.kind() != Kind.FILLER && map.put(f.name(), f) != null) {
                throw new IllegalArgumentException(name + ": duplicate field " + f.name());
            }
        }
        this.byName = Collections.unmodifiableMap(map);
    }

    public static Builder builder(String name, int length) {
        return new Builder(name, length);
    }

    public String name() {
        return name;
    }

    public int length() {
        return length;
    }

    public List<Field> fields() {
        return fields;
    }

    public Field field(String fieldName) {
        Field f = byName.get(fieldName);
        if (f == null) {
            throw new IllegalArgumentException(name + " has no field " + fieldName);
        }
        return f;
    }

    /** Wraps one record; the text must be exactly {@link #length()} characters. */
    public FixedRecord parse(String text) {
        if (text.length() != length) {
            throw new IllegalArgumentException(name + " record must be " + length + " characters, got " + text.length());
        }
        return new FixedRecord(this, new StringBuilder(text));
    }

    /** A record with every field at its initial value: spaces, zeros, FILLER fill. */
    public FixedRecord blank() {
        StringBuilder sb = new StringBuilder(length);
        for (Field f : fields) {
            sb.append(switch (f.kind()) {
                case ALPHA -> " ".repeat(f.length());
                case UNSIGNED, SIGNED -> ZonedDecimal.encode(BigDecimal.ZERO, f.length(), f.scale(), f.kind() == Kind.SIGNED);
                case FILLER -> String.valueOf(f.fill()).repeat(f.length());
            });
        }
        return new FixedRecord(this, sb);
    }

    public static final class Builder {
        private final String name;
        private final int length;
        private final List<Field> fields = new ArrayList<>();
        private int offset;

        private Builder(String name, int length) {
            this.name = name;
            this.length = length;
        }

        /** {@code PIC X(n)}. */
        public Builder alpha(String field, int n) {
            return add(field, Kind.ALPHA, n, 0, ' ');
        }

        /** {@code PIC 9(n)}. */
        public Builder unsigned(String field, int n) {
            return add(field, Kind.UNSIGNED, n, 0, '0');
        }

        /** {@code PIC S9(integer)V9(scale)}, overpunched sign. */
        public Builder signed(String field, int integer, int scale) {
            return add(field, Kind.SIGNED, integer + scale, scale, '0');
        }

        /** {@code FILLER PIC X(n)}, emitted as spaces. */
        public Builder filler(int n) {
            return filler(n, ' ');
        }

        /** {@code FILLER PIC X(n)}, emitted as {@code fill}. */
        public Builder filler(int n, char fill) {
            return add("FILLER", Kind.FILLER, n, 0, fill);
        }

        private Builder add(String field, Kind kind, int n, int scale, char fill) {
            fields.add(new Field(field, kind, offset, n, scale, fill));
            offset += n;
            return this;
        }

        public RecordLayout build() {
            if (offset != length) {
                throw new IllegalStateException(name + ": fields add up to " + offset + ", record length is " + length);
            }
            return new RecordLayout(name, length, fields);
        }
    }
}
