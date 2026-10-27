package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.RecordFields;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.charset.Charset;

/**
 * 50-byte CVTRA01Y layout.
 * @param accountId app/cpy/CVTRA01Y.cpy:6, PIC 9(11)
 * @param typeCode app/cpy/CVTRA01Y.cpy:7, PIC X(2)
 * @param categoryCode app/cpy/CVTRA01Y.cpy:8, PIC 9(4)
 * @param balance app/cpy/CVTRA01Y.cpy:9, PIC S9(9)V99
 * @param filler app/cpy/CVTRA01Y.cpy:10, PIC X(22)
 */
public record TranCatBalanceRecord(
        String accountId,
        String typeCode,
        String categoryCode,
        ZonedDecimal balance,
        String filler) {
    public static final RecordCodec<TranCatBalanceRecord> CODEC = new RecordCodec<>() {
        public int length() { return 50; }
        public TranCatBalanceRecord decode(byte[] bytes, Charset charset) {
            RecordFields fields = new RecordFields(bytes, length(), charset);
            return new TranCatBalanceRecord(
                    fields.unsigned(11),
                    fields.text(2),
                    fields.unsigned(4),
                    fields.money(11),
                    fields.text(22));
        }
        public byte[] encode(TranCatBalanceRecord record, Charset charset) {
            RecordFields fields = new RecordFields(null, length(), charset);
            fields.unsigned(record.accountId(), 11);
            fields.text(record.typeCode(), 2);
            fields.unsigned(record.categoryCode(), 4);
            fields.money(record.balance(), 11);
            fields.text(record.filler(), 22);
            return fields.bytes();
        }
    };
}
