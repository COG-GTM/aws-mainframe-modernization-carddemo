package com.carddemo.modernization.records;

import com.carddemo.modernization.records.codec.RecordCodec;
import com.carddemo.modernization.records.codec.RecordFields;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.nio.charset.Charset;

/**
 * 300-byte CVACT01Y layout.
 * @param acctId app/cpy/CVACT01Y.cpy:5, PIC 9(11)
 * @param activeStatus app/cpy/CVACT01Y.cpy:6, PIC X(1)
 * @param currentBalance app/cpy/CVACT01Y.cpy:7, PIC S9(10)V99
 * @param creditLimit app/cpy/CVACT01Y.cpy:8, PIC S9(10)V99
 * @param cashCreditLimit app/cpy/CVACT01Y.cpy:9, PIC S9(10)V99
 * @param openDate app/cpy/CVACT01Y.cpy:10, PIC X(10)
 * @param expirationDate app/cpy/CVACT01Y.cpy:11, PIC X(10)
 * @param reissueDate app/cpy/CVACT01Y.cpy:12, PIC X(10)
 * @param cycleCredit app/cpy/CVACT01Y.cpy:13, PIC S9(10)V99
 * @param cycleDebit app/cpy/CVACT01Y.cpy:14, PIC S9(10)V99
 * @param addressZip app/cpy/CVACT01Y.cpy:15, PIC X(10)
 * @param groupId app/cpy/CVACT01Y.cpy:16, PIC X(10)
 * @param filler app/cpy/CVACT01Y.cpy:17, PIC X(178)
 */
public record AccountRecord(
        String acctId,
        String activeStatus,
        ZonedDecimal currentBalance,
        ZonedDecimal creditLimit,
        ZonedDecimal cashCreditLimit,
        String openDate,
        String expirationDate,
        String reissueDate,
        ZonedDecimal cycleCredit,
        ZonedDecimal cycleDebit,
        String addressZip,
        String groupId,
        String filler) {
    public static final RecordCodec<AccountRecord> CODEC = new RecordCodec<>() {
        public int length() { return 300; }
        public AccountRecord decode(byte[] bytes, Charset charset) {
            RecordFields fields = new RecordFields(bytes, length(), charset);
            return new AccountRecord(
                    fields.unsigned(11),
                    fields.text(1),
                    fields.money(12),
                    fields.money(12),
                    fields.money(12),
                    fields.text(10),
                    fields.text(10),
                    fields.text(10),
                    fields.money(12),
                    fields.money(12),
                    fields.text(10),
                    fields.text(10),
                    fields.text(178));
        }
        public byte[] encode(AccountRecord record, Charset charset) {
            RecordFields fields = new RecordFields(null, length(), charset);
            fields.unsigned(record.acctId(), 11);
            fields.text(record.activeStatus(), 1);
            fields.money(record.currentBalance(), 12);
            fields.money(record.creditLimit(), 12);
            fields.money(record.cashCreditLimit(), 12);
            fields.text(record.openDate(), 10);
            fields.text(record.expirationDate(), 10);
            fields.text(record.reissueDate(), 10);
            fields.money(record.cycleCredit(), 12);
            fields.money(record.cycleDebit(), 12);
            fields.text(record.addressZip(), 10);
            fields.text(record.groupId(), 10);
            fields.text(record.filler(), 178);
            return fields.bytes();
        }
    };
}
