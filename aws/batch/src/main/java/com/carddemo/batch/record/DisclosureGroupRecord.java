package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.pad;

import java.math.BigDecimal;

/** {@code CVTRA02Y} DIS-GROUP-RECORD (50 bytes). */
public record DisclosureGroupRecord(String acctGroupId, String typeCd, int catCd, BigDecimal intRate) {

    public static final int LENGTH = 50;

    public static DisclosureGroupRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new DisclosureGroupRecord(Fixed.rtrim(field(r, 1, 10)), field(r, 11, 2),
                (int) Zoned.parseUnsigned(field(r, 13, 4)), Zoned.parseSigned(field(r, 17, 6), 2));
    }

    public String format() {
        return pad(acctGroupId, 10) + pad(typeCd, 2) + Zoned.formatUnsigned(catCd, 4)
                + Zoned.formatSigned(intRate, 4, 2) + " ".repeat(28);
    }
}
