package com.carddemo.common.record;

import com.carddemo.common.cobol.ZonedDecimal;
import com.carddemo.common.record.RecordLayout.Field;
import com.carddemo.common.record.RecordLayout.Kind;

import java.math.BigDecimal;

/** One record of a {@link RecordLayout}; typed accessors decode and encode the fixed-width text. */
public final class FixedRecord {

    private final RecordLayout layout;
    private final StringBuilder text;

    FixedRecord(RecordLayout layout, StringBuilder text) {
        this.layout = layout;
        this.text = text;
    }

    public RecordLayout layout() {
        return layout;
    }

    /** {@code PIC X(n)} value exactly as stored (trailing spaces kept). */
    public String raw(String field) {
        Field f = layout.field(field);
        return text.substring(f.offset(), f.end());
    }

    /** {@code PIC X(n)} value with trailing spaces removed. */
    public String text(String field) {
        return raw(field).stripTrailing();
    }

    public long number(String field) {
        return decimal(field).longValueExact();
    }

    public BigDecimal decimal(String field) {
        Field f = layout.field(field);
        if (f.kind() != Kind.UNSIGNED && f.kind() != Kind.SIGNED) {
            throw new IllegalArgumentException(layout.name() + "." + field + " is not numeric");
        }
        try {
            return ZonedDecimal.decode(raw(field), f.scale(), f.kind() == Kind.SIGNED);
        } catch (NumberFormatException e) {
            throw new NumberFormatException(layout.name() + "." + field + ": " + e.getMessage());
        }
    }

    /** Right-pads with spaces; refuses values longer than the field. */
    public FixedRecord set(String field, String value) {
        Field f = layout.field(field);
        if (f.kind() != Kind.ALPHA) {
            throw new IllegalArgumentException(layout.name() + "." + field + " is not alphanumeric");
        }
        String v = value == null ? "" : value;
        if (v.length() > f.length()) {
            throw new IllegalArgumentException(layout.name() + "." + field + " is " + f.length() + " characters: '" + v + "'");
        }
        text.replace(f.offset(), f.end(), v + " ".repeat(f.length() - v.length()));
        return this;
    }

    public FixedRecord set(String field, long value) {
        return set(field, BigDecimal.valueOf(value));
    }

    public FixedRecord set(String field, BigDecimal value) {
        Field f = layout.field(field);
        if (f.kind() != Kind.UNSIGNED && f.kind() != Kind.SIGNED) {
            throw new IllegalArgumentException(layout.name() + "." + field + " is not numeric");
        }
        text.replace(f.offset(), f.end(), ZonedDecimal.encode(value, f.length(), f.scale(), f.kind() == Kind.SIGNED));
        return this;
    }

    @Override
    public String toString() {
        return text.toString();
    }
}
