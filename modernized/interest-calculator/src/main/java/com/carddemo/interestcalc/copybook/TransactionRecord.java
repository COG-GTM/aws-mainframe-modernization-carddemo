package com.carddemo.interestcalc.copybook;

import java.math.BigDecimal;

/**
 * Copybook {@code CVTRA05Y} - {@code TRAN-RECORD}, RECLN 350. This is the TRANSACT output
 * record written by {@code 1300-B-WRITE-TX}.
 *
 * <pre>
 * 05 TRAN-ID             PIC X(16).      bytes   1-16
 * 05 TRAN-TYPE-CD        PIC X(02).      bytes  17-18
 * 05 TRAN-CAT-CD         PIC 9(04).      bytes  19-22
 * 05 TRAN-SOURCE         PIC X(10).      bytes  23-32
 * 05 TRAN-DESC           PIC X(100).     bytes  33-132
 * 05 TRAN-AMT            PIC S9(09)V99.  bytes 133-143
 * 05 TRAN-MERCHANT-ID    PIC 9(09).      bytes 144-152
 * 05 TRAN-MERCHANT-NAME  PIC X(50).      bytes 153-202
 * 05 TRAN-MERCHANT-CITY  PIC X(50).      bytes 203-252
 * 05 TRAN-MERCHANT-ZIP   PIC X(10).      bytes 253-262
 * 05 TRAN-CARD-NUM       PIC X(16).      bytes 263-278
 * 05 TRAN-ORIG-TS        PIC X(26).      bytes 279-304
 * 05 TRAN-PROC-TS        PIC X(26).      bytes 305-330
 * 05 FILLER              PIC X(20).      bytes 331-350
 * </pre>
 */
public record TransactionRecord(String transactionId, String typeCode, String categoryCode, String source,
                                String description, BigDecimal amount, String merchantId, String merchantName,
                                String merchantCity, String merchantZip, String cardNumber,
                                String originTimestamp, String processTimestamp) {

    public static final int LENGTH = 350;

    public static TransactionRecord parse(String raw) {
        String rec = FixedWidth.record(raw, LENGTH);
        return new TransactionRecord(
                FixedWidth.field(rec, 0, 16),
                FixedWidth.field(rec, 16, 2),
                FixedWidth.field(rec, 18, 4),
                FixedWidth.field(rec, 22, 10),
                FixedWidth.field(rec, 32, 100),
                ZonedDecimal.decode(FixedWidth.field(rec, 132, 11), 9, 2),
                FixedWidth.field(rec, 143, 9),
                FixedWidth.field(rec, 152, 50),
                FixedWidth.field(rec, 202, 50),
                FixedWidth.field(rec, 252, 10),
                FixedWidth.field(rec, 262, 16),
                FixedWidth.field(rec, 278, 26),
                FixedWidth.field(rec, 304, 26));
    }

    public String format() {
        return FixedWidth.alphanumeric(transactionId, 16)
                + FixedWidth.alphanumeric(typeCode, 2)
                + FixedWidth.unsigned(categoryCode, 4)
                + FixedWidth.alphanumeric(source, 10)
                + FixedWidth.alphanumeric(description, 100)
                + ZonedDecimal.encode(amount, 9, 2)
                + FixedWidth.unsigned(merchantId, 9)
                + FixedWidth.alphanumeric(merchantName, 50)
                + FixedWidth.alphanumeric(merchantCity, 50)
                + FixedWidth.alphanumeric(merchantZip, 10)
                + FixedWidth.alphanumeric(cardNumber, 16)
                + FixedWidth.alphanumeric(originTimestamp, 26)
                + FixedWidth.alphanumeric(processTimestamp, 26)
                + " ".repeat(20);
    }
}
