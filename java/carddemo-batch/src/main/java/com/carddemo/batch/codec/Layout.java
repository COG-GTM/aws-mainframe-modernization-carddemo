package com.carddemo.batch.codec;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * A copybook record layout: an ordered list of {@link Field}s whose offsets are derived from the
 * copybook order, exactly like the COBOL compiler lays out a record.
 *
 * <pre>
 *   Layout.Builder b = Layout.builder("ACCOUNT-RECORD");
 *   static final Field ACCT_ID = b.unsigned("ACCT-ID", 11);      // PIC 9(11)
 *   static final Field ACCT_CURR_BAL = b.zoned("ACCT-CURR-BAL", 10, 2); // PIC S9(10)V99
 *   static final Layout LAYOUT = b.filler(178).build();
 * </pre>
 */
public final class Layout {

    private final String name;
    private final int length;
    private final List<Field> fields;

    private Layout(String name, int length, List<Field> fields) {
        this.name = name;
        this.length = length;
        this.fields = List.copyOf(fields);
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
        for (Field f : fields) {
            if (f.name().equals(fieldName)) {
                return f;
            }
        }
        throw new IllegalArgumentException("no field " + fieldName + " in " + name);
    }

    public static Builder builder(String name) {
        return new Builder(name, 0);
    }

    public static final class Builder {
        private final String name;
        private final int base;
        private int cursor;
        private final List<Field> fields = new ArrayList<>();

        private Builder(String name, int base) {
            this.name = name;
            this.base = base;
            this.cursor = base;
        }

        /** PIC X(len). */
        public Field text(String fieldName, int len) {
            return add(new Field(fieldName, cursor, len, Field.Usage.TEXT, 0, 0, false, 1, null));
        }

        /** PIC 9(digits) USAGE DISPLAY (unsigned, no decimals). */
        public Field unsigned(String fieldName, int digits) {
            return add(new Field(fieldName, cursor, digits, Field.Usage.ZONED, digits, 0, false, 1, null));
        }

        /** PIC S9(intDigits)V9(scale) USAGE DISPLAY. */
        public Field zoned(String fieldName, int intDigits, int scale) {
            int digits = intDigits + scale;
            return add(new Field(fieldName, cursor, digits, Field.Usage.ZONED, digits, scale, true, 1, null));
        }

        /** PIC S9(intDigits)V9(scale) USAGE COMP-3. */
        public Field packed(String fieldName, int intDigits, int scale) {
            int digits = intDigits + scale;
            return add(new Field(fieldName, cursor, PackedDecimal.bytesFor(digits), Field.Usage.PACKED,
                    digits, scale, true, 1, null));
        }

        /** FILLER PIC X(len). */
        public Field filler(int len) {
            return add(new Field("FILLER@" + cursor, cursor, len, Field.Usage.FILLER, 0, 0, false, 1, null));
        }

        /** A group item {@code OCCURS occurs TIMES}; {@code body} declares the subordinate items in order. */
        public Field group(String fieldName, int occurs, Consumer<Builder> body) {
            Builder sub = new Builder(fieldName, cursor);
            body.accept(sub);
            int one = sub.cursor - cursor;
            Field g = new Field(fieldName, cursor, one, Field.Usage.GROUP, 0, 0, false, occurs, sub.fields);
            fields.add(g);
            cursor += one * occurs;
            return g;
        }

        public Layout build() {
            return new Layout(name, cursor - base, fields);
        }

        private Field add(Field f) {
            fields.add(f);
            cursor += f.length();
            return f;
        }
    }
}
