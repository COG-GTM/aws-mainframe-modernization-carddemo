package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.RecordFields;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.charset.Charset;

/**
 * 50-byte CVTRA02Y layout.
 * @param groupId app/cpy/CVTRA02Y.cpy:6, PIC X(10)
 * @param typeCode app/cpy/CVTRA02Y.cpy:7, PIC X(2)
 * @param categoryCode app/cpy/CVTRA02Y.cpy:8, PIC 9(4)
 * @param interestRate app/cpy/CVTRA02Y.cpy:9, PIC S9(4)V99
 * @param filler app/cpy/CVTRA02Y.cpy:10, PIC X(28)
 */
public record DisclosureGroupRecord(
        String groupId,
        String typeCode,
        String categoryCode,
        ZonedDecimal interestRate,
        String filler) {
    public static final RecordCodec<DisclosureGroupRecord> CODEC = new RecordCodec<>() {
        public int length() { return 50; }
        public DisclosureGroupRecord decode(byte[] bytes, Charset charset) {
            RecordFields fields = new RecordFields(bytes, length(), charset);
            return new DisclosureGroupRecord(
                    fields.text(10),
                    fields.text(2),
                    fields.unsigned(4),
                    fields.money(6),
                    fields.text(28));
        }
        public byte[] encode(DisclosureGroupRecord record, Charset charset) {
            RecordFields fields = new RecordFields(null, length(), charset);
            fields.text(record.groupId(), 10);
            fields.text(record.typeCode(), 2);
            fields.unsigned(record.categoryCode(), 4);
            fields.money(record.interestRate(), 6);
            fields.text(record.filler(), 28);
            return fields.bytes();
        }
    };
}
