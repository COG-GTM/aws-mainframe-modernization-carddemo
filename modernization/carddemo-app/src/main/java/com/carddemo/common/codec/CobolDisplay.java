package com.carddemo.common.codec;

import java.math.BigDecimal;

/**
 * What {@code DISPLAY item} prints for an elementary item under GnuCOBOL (the phase-1 baseline): alphanumeric items
 * as their characters, unsigned zoned items as their digits, signed numeric items as their digits followed by a
 * separate {@code +}/{@code -} (e.g. {@code PIC S9(10)V99} 194.00 prints {@code 000000019400+}). The implied
 * decimal point is not printed. A group item prints its bytes, i.e. {@link FixedWidthRecord#text()}.
 */
public final class CobolDisplay {

    private CobolDisplay() {
    }

    public static String of(FixedWidthRecord record, Field field) {
        if (!field.isNumeric() || field.isGroup()) {
            return record.getString(field);
        }
        return numeric(record.getDecimal(field), field.digits(), field.scale(), field.signed());
    }

    public static String of(FixedWidthRecord record, String name) {
        return of(record, record.field(name));
    }

    /** {@code DISPLAY} of a numeric value held in an item of {@code digits} digits ({@code scale} after the point). */
    public static String numeric(BigDecimal value, int digits, int scale, boolean signed) {
        BigDecimal stored = CobolNumeric.truncate(value, digits, scale, signed);
        String text = stored.unscaledValue().abs().toString();
        String padded = "0".repeat(Math.max(0, digits - text.length())) + text;
        return signed ? padded + (stored.signum() < 0 ? '-' : '+') : padded;
    }
}
