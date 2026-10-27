package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.RecordFields;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.charset.Charset;

/**
 * 350-byte CVTRA06Y layout.
 * @param id app/cpy/CVTRA06Y.cpy:5, PIC X(16)
 * @param typeCode app/cpy/CVTRA06Y.cpy:6, PIC X(2)
 * @param categoryCode app/cpy/CVTRA06Y.cpy:7, PIC 9(4)
 * @param source app/cpy/CVTRA06Y.cpy:8, PIC X(10)
 * @param description app/cpy/CVTRA06Y.cpy:9, PIC X(100)
 * @param amount app/cpy/CVTRA06Y.cpy:10, PIC S9(9)V99
 * @param merchantId app/cpy/CVTRA06Y.cpy:11, PIC 9(9)
 * @param merchantName app/cpy/CVTRA06Y.cpy:12, PIC X(50)
 * @param merchantCity app/cpy/CVTRA06Y.cpy:13, PIC X(50)
 * @param merchantZip app/cpy/CVTRA06Y.cpy:14, PIC X(10)
 * @param cardNumber app/cpy/CVTRA06Y.cpy:15, PIC X(16)
 * @param originalTimestamp app/cpy/CVTRA06Y.cpy:16, PIC X(26)
 * @param processedTimestamp app/cpy/CVTRA06Y.cpy:17, PIC X(26)
 * @param filler app/cpy/CVTRA06Y.cpy:18, PIC X(20)
 */
public record DailyTransactionRecord(
        String id,
        String typeCode,
        String categoryCode,
        String source,
        String description,
        ZonedDecimal amount,
        String merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNumber,
        String originalTimestamp,
        String processedTimestamp,
        String filler) {
    public static final RecordCodec<DailyTransactionRecord> CODEC = new RecordCodec<>() {
        public int length() { return 350; }
        public DailyTransactionRecord decode(byte[] bytes, Charset charset) {
            RecordFields fields = new RecordFields(bytes, length(), charset);
            return new DailyTransactionRecord(
                    fields.text(16),
                    fields.text(2),
                    fields.unsigned(4),
                    fields.text(10),
                    fields.text(100),
                    fields.money(11),
                    fields.unsigned(9),
                    fields.text(50),
                    fields.text(50),
                    fields.text(10),
                    fields.text(16),
                    fields.text(26),
                    fields.text(26),
                    fields.text(20));
        }
        public byte[] encode(DailyTransactionRecord record, Charset charset) {
            RecordFields fields = new RecordFields(null, length(), charset);
            fields.text(record.id(), 16);
            fields.text(record.typeCode(), 2);
            fields.unsigned(record.categoryCode(), 4);
            fields.text(record.source(), 10);
            fields.text(record.description(), 100);
            fields.money(record.amount(), 11);
            fields.unsigned(record.merchantId(), 9);
            fields.text(record.merchantName(), 50);
            fields.text(record.merchantCity(), 50);
            fields.text(record.merchantZip(), 10);
            fields.text(record.cardNumber(), 16);
            fields.text(record.originalTimestamp(), 26);
            fields.text(record.processedTimestamp(), 26);
            fields.text(record.filler(), 20);
            return fields.bytes();
        }
    };
}
