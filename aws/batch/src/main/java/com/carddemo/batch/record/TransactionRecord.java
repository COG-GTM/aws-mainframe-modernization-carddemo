package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.pad;
import static com.carddemo.batch.record.Fixed.text;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/** {@code CVTRA05Y} TRAN-RECORD / {@code CVTRA06Y} DALYTRAN-RECORD (identical 350-byte layouts). */
public record TransactionRecord(
        String tranId,
        String typeCd,
        int catCd,
        String source,
        String description,
        BigDecimal amt,
        Integer merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNum,
        LocalDateTime origTs,
        LocalDateTime procTs) {

    public static final int LENGTH = 350;

    public static TransactionRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new TransactionRecord(
                field(r, 1, 16),
                field(r, 17, 2),
                (int) Zoned.parseUnsigned(field(r, 19, 4)),
                text(field(r, 23, 10)),
                text(field(r, 33, 100)),
                Zoned.parseSigned(field(r, 133, 11), 2),
                (int) Zoned.parseUnsigned(field(r, 144, 9)),
                text(field(r, 153, 50)),
                text(field(r, 203, 50)),
                text(field(r, 253, 10)),
                field(r, 263, 16),
                Fixed.timestamp(field(r, 279, 26)),
                Fixed.timestamp(field(r, 305, 26)));
    }

    /** Legacy 350-byte layout; timestamps rendered with {@code tsFormat}. */
    public String format(DateTimeFormatter tsFormat) {
        return pad(tranId, 16)
                + pad(typeCd, 2)
                + Zoned.formatUnsigned(catCd, 4)
                + pad(source, 10)
                + pad(description, 100)
                + Zoned.formatSigned(amt, 9, 2)
                + Zoned.formatUnsigned(merchantId == null ? 0 : merchantId, 9)
                + pad(merchantName, 50)
                + pad(merchantCity, 50)
                + pad(merchantZip, 10)
                + pad(cardNum, 16)
                + Fixed.timestamp(origTs, tsFormat)
                + Fixed.timestamp(procTs, tsFormat)
                + " ".repeat(20);
    }
}
