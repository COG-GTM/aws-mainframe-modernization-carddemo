package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.pad;

import java.math.BigDecimal;

/** {@code CVTRA01Y} TRAN-CAT-BAL-RECORD (50 bytes). */
public record TranCatBalRecord(long acctId, String typeCd, int catCd, BigDecimal balance) {

    public static final int LENGTH = 50;

    public static TranCatBalRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new TranCatBalRecord(Zoned.parseUnsigned(field(r, 1, 11)), field(r, 12, 2),
                (int) Zoned.parseUnsigned(field(r, 14, 4)), Zoned.parseSigned(field(r, 18, 11), 2));
    }

    public String format() {
        return Zoned.formatUnsigned(acctId, 11) + pad(typeCd, 2) + Zoned.formatUnsigned(catCd, 4)
                + Zoned.formatSigned(balance, 9, 2) + " ".repeat(22);
    }
}
