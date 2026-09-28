package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;

/** {@code CVACT03Y} CARD-XREF-RECORD (50 bytes; ASCII sample rows omit the 14-byte filler). */
public record CardXrefRecord(String cardNum, int custId, long acctId) {

    public static final int LENGTH = 50;

    public static CardXrefRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new CardXrefRecord(field(r, 1, 16), (int) Zoned.parseUnsigned(field(r, 17, 9)),
                Zoned.parseUnsigned(field(r, 26, 11)));
    }
}
