package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldReader;
import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

import java.math.BigDecimal;

/** {@code DIS-GROUP-RECORD} from copybook CVTRA02Y (50 bytes), DISCGRP KSDS keyed by {@code DIS-GROUP-KEY}. */
public record DisclosureGroupRecord(
        DisGroupKey disGroupKey,
        BigDecimal disIntRate,   // PIC S9(04)V99
        String filler) {         // PIC X(28)

    public static final int LENGTH = 50;

    /** {@code DIS-GROUP-KEY} (16 bytes); ordered like the VSAM key bytes. */
    public record DisGroupKey(
            String disAcctGroupId,   // PIC X(10)
            String disTranTypeCd,    // PIC X(02)
            int disTranCatCd)        // PIC 9(04)
            implements Comparable<DisGroupKey> {

        public static final int LENGTH = 16;

        static DisGroupKey read(FieldReader r) {
            return new DisGroupKey(r.text(10), r.text(2), r.unsignedInt(4));
        }

        FieldWriter write(FieldWriter w) {
            return w.text(disAcctGroupId, 10).text(disTranTypeCd, 2).unsigned(disTranCatCd, 4);
        }

        public String encode() {
            return write(new FieldWriter(LENGTH)).toString();
        }

        @Override
        public int compareTo(DisGroupKey o) {
            return encode().compareTo(o.encode());
        }
    }

    public static final FixedWidthCodec<DisclosureGroupRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new DisclosureGroupRecord(DisGroupKey.read(r), r.signedDecimal(6, 2), r.text(28)),
            d -> d.disGroupKey.write(new FieldWriter(LENGTH))
                    .signedDecimal(d.disIntRate, 6, 2)
                    .text(d.filler, 28)
                    .toString());
}
