package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;

import java.time.LocalDate;

/** {@code CVACT02Y} CARD-RECORD (150 bytes). */
public record CardRecord(String cardNum, long acctId, int cvvCd, String embossedName, LocalDate expirationDate,
        String activeStatus) {

    public static final int LENGTH = 150;

    public static CardRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new CardRecord(field(r, 1, 16), Zoned.parseUnsigned(field(r, 17, 11)),
                (int) Zoned.parseUnsigned(field(r, 28, 3)), Fixed.rtrim(field(r, 31, 50)),
                Fixed.date(field(r, 81, 10)), field(r, 91, 1));
    }
}
