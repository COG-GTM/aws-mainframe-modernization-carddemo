package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.pad;

/** {@code CVTRA03Y} TRAN-TYPE-RECORD (60 bytes) = TRANEXTR STEP40 unload layout. */
public record TransactionTypeRecord(String typeCd, String description) {

    public static final int LENGTH = 60;

    public static TransactionTypeRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new TransactionTypeRecord(field(r, 1, 2), Fixed.rtrim(field(r, 3, 50)));
    }

    /** {@code TR_TYPE || CAST(TR_DESCRIPTION AS CHAR(50)) || REPEAT('0',8)}. */
    public String format() {
        return pad(typeCd, 2) + pad(description, 50) + "00000000";
    }
}
