package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.RecordFields;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.charset.Charset;

/**
 * 50-byte CVACT03Y layout.
 * @param cardNumber app/cpy/CVACT03Y.cpy:5, PIC X(16)
 * @param customerId app/cpy/CVACT03Y.cpy:6, PIC 9(9)
 * @param accountId app/cpy/CVACT03Y.cpy:7, PIC 9(11)
 * @param filler app/cpy/CVACT03Y.cpy:8, PIC X(14)
 */
public record CardXrefRecord(
        String cardNumber,
        String customerId,
        String accountId,
        String filler) {
    public static final RecordCodec<CardXrefRecord> CODEC = new RecordCodec<>() {
        public int length() { return 50; }
        public CardXrefRecord decode(byte[] bytes, Charset charset) {
            RecordFields fields = new RecordFields(bytes, length(), charset);
            return new CardXrefRecord(
                    fields.text(16),
                    fields.unsigned(9),
                    fields.unsigned(11),
                    fields.text(14));
        }
        public byte[] encode(CardXrefRecord record, Charset charset) {
            RecordFields fields = new RecordFields(null, length(), charset);
            fields.text(record.cardNumber(), 16);
            fields.unsigned(record.customerId(), 9);
            fields.unsigned(record.accountId(), 11);
            fields.text(record.filler(), 14);
            return fields.bytes();
        }
    };
}
