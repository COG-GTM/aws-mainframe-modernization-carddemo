package com.carddemo.runtime.cobol;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Describes a copybook record layout: elementary items laid out sequentially, with support for
 * FILLER, group items and REDEFINES. Built once (static) per copybook.
 *
 * <pre>{@code
 * RecordLayout.Builder b = RecordLayout.builder("SEC-USER-DATA");
 * AlphanumericField id = b.x("SEC-USR-ID", 8);
 * NumericField amt = b.packed("AMT", 9, 2, true);
 * RecordLayout layout = b.build();
 * }</pre>
 */
public final class RecordLayout {

    private final String name;
    private final int length;
    private final List<CobolField> elementaryFields;
    private final Map<String, CobolField> byName;

    private RecordLayout(String name, int length, List<CobolField> elementary, Map<String, CobolField> byName) {
        this.name = name;
        this.length = length;
        this.elementaryFields = Collections.unmodifiableList(elementary);
        this.byName = Collections.unmodifiableMap(byName);
    }

    public static Builder builder(String name) {
        return new Builder(name);
    }

    public String name() {
        return name;
    }

    public int length() {
        return length;
    }

    /** Elementary items in declaration order (FILLER excluded, REDEFINES included). */
    public List<CobolField> fields() {
        return elementaryFields;
    }

    /** Any named item (elementary or group). */
    public CobolField field(String fieldName) {
        CobolField f = byName.get(fieldName);
        if (f == null) {
            throw new IllegalArgumentException(name + " has no field " + fieldName);
        }
        return f;
    }

    /**
     * Field-name to value view of a record (for diagnostics and JSON): alphanumerics as strings, numerics as
     * BigDecimal, LOW-VALUES items as null, invalid numerics as their raw text.
     */
    public Map<String, Object> describe(CobolRecord record) {
        Map<String, Object> out = new LinkedHashMap<>();
        for (CobolField f : elementaryFields) {
            out.put(f.name(), f.describe(record));
        }
        return out;
    }

    /** Builder that assigns offsets sequentially. */
    public static final class Builder {
        private final String name;
        private final List<CobolField> elementary = new ArrayList<>();
        private final Map<String, CobolField> byName = new LinkedHashMap<>();
        private final Deque<Object[]> groups = new ArrayDeque<>();
        private int offset;
        private int maxOffset;

        private Builder(String name) {
            this.name = name;
        }

        public AlphanumericField x(String fieldName, int len) {
            return add(new AlphanumericField(fieldName, offset, len));
        }

        /** PIC 9(n)V9(s) / S9 DISPLAY. */
        public NumericField zoned(String fieldName, int integerDigits, int scale, boolean signed) {
            return add(new NumericField(fieldName, offset, NumericField.Usage.DISPLAY, integerDigits, scale, signed));
        }

        /** PIC S9(n)V9(s) COMP-3. */
        public NumericField packed(String fieldName, int integerDigits, int scale, boolean signed) {
            return add(new NumericField(fieldName, offset, NumericField.Usage.COMP_3, integerDigits, scale, signed));
        }

        /** PIC S9(n) COMP / BINARY. */
        public NumericField binary(String fieldName, int integerDigits, int scale, boolean signed) {
            return add(new NumericField(fieldName, offset, NumericField.Usage.COMP, integerDigits, scale, signed));
        }

        public Builder filler(int len) {
            advance(len);
            return this;
        }

        /** Starts a group item; close it with {@link #endGroup()}. */
        public Builder group(String groupName) {
            groups.push(new Object[] {groupName, offset});
            return this;
        }

        /** Ends the innermost group and returns an alphanumeric view of it. */
        public AlphanumericField endGroup() {
            Object[] g = groups.pop();
            int start = (Integer) g[1];
            AlphanumericField f = new AlphanumericField((String) g[0], start, offset - start);
            byName.put(f.name(), f);
            return f;
        }

        /** Repositions at the start of {@code target} so the following items REDEFINE it. */
        public Builder redefines(CobolField target) {
            offset = target.offset();
            return this;
        }

        /** Repositions after the longest of the redefined alternatives. */
        public Builder endRedefines(CobolField target) {
            offset = Math.max(offset, target.offset() + target.length());
            return this;
        }

        public RecordLayout build() {
            if (!groups.isEmpty()) {
                throw new IllegalStateException("Unclosed group in " + name);
            }
            return new RecordLayout(name, maxOffset, new ArrayList<>(elementary), new LinkedHashMap<>(byName));
        }

        private <F extends CobolField> F add(F field) {
            if (byName.containsKey(field.name())) {
                throw new IllegalArgumentException("Duplicate field " + field.name() + " in " + name);
            }
            elementary.add(field);
            byName.put(field.name(), field);
            advance(field.length());
            return field;
        }

        private void advance(int len) {
            offset += len;
            maxOffset = Math.max(maxOffset, offset);
        }
    }
}
