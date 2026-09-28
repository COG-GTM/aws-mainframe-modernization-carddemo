package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.pad;

/** {@code CVTRA04Y} TRAN-CAT-RECORD (60 bytes) = TRANEXTR STEP50 unload layout. */
public record TransactionCategoryRecord(String typeCd, int catCd, String description) {

    public static final int LENGTH = 60;

    public static TransactionCategoryRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new TransactionCategoryRecord(field(r, 1, 2), (int) Zoned.parseUnsigned(field(r, 3, 4)),
                Fixed.rtrim(field(r, 7, 50)));
    }

    /** {@code TRC_TYPE_CODE || TRC_TYPE_CATEGORY || CAST(TRC_CAT_DATA AS CHAR(50)) || REPEAT('0',4)}. */
    public String format() {
        return pad(typeCd, 2) + Zoned.formatUnsigned(catCd, 4) + pad(description, 50) + "0000";
    }
}
