package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

/**
 * {@code CARD-XREF-RECORD} from copybook CVACT03Y (50 bytes), CARDXREF KSDS keyed by {@code XREF-CARD-NUM}
 * with an alternate index on {@code XREF-ACCT-ID}. {@code cardxref.txt} stores it with the trailing FILLER trimmed.
 */
public record CardXrefRecord(
        String xrefCardNum,   // PIC X(16)
        long xrefCustId,      // PIC 9(09)
        long xrefAcctId,      // PIC 9(11)
        String filler) {      // PIC X(14)

    public static final int LENGTH = 50;

    public static final FixedWidthCodec<CardXrefRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new CardXrefRecord(r.text(16), r.unsignedLong(9), r.unsignedLong(11), r.text(14)),
            x -> new FieldWriter(LENGTH)
                    .text(x.xrefCardNum, 16)
                    .unsigned(x.xrefCustId, 9)
                    .unsigned(x.xrefAcctId, 11)
                    .text(x.filler, 14)
                    .toString());
}
