package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.pad;
import static com.carddemo.batch.record.Fixed.text;

import java.math.BigDecimal;
import java.time.LocalDate;

/** {@code CVACT01Y} ACCOUNT-RECORD (300 bytes). */
public record AccountRecord(
        long acctId,
        String activeStatus,
        BigDecimal currBal,
        BigDecimal creditLimit,
        BigDecimal cashCreditLimit,
        LocalDate openDate,
        LocalDate expirationDate,
        LocalDate reissueDate,
        BigDecimal currCycCredit,
        BigDecimal currCycDebit,
        String addrZip,
        String groupId) {

    public static final int LENGTH = 300;

    public static AccountRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        return new AccountRecord(
                Zoned.parseUnsigned(field(r, 1, 11)),
                field(r, 12, 1),
                Zoned.parseSigned(field(r, 13, 12), 2),
                Zoned.parseSigned(field(r, 25, 12), 2),
                Zoned.parseSigned(field(r, 37, 12), 2),
                Fixed.date(field(r, 49, 10)),
                Fixed.date(field(r, 59, 10)),
                Fixed.date(field(r, 69, 10)),
                Zoned.parseSigned(field(r, 79, 12), 2),
                Zoned.parseSigned(field(r, 91, 12), 2),
                text(field(r, 103, 10)),
                text(field(r, 113, 10)));
    }

    public String format() {
        return Zoned.formatUnsigned(acctId, 11)
                + pad(activeStatus, 1)
                + Zoned.formatSigned(currBal, 10, 2)
                + Zoned.formatSigned(creditLimit, 10, 2)
                + Zoned.formatSigned(cashCreditLimit, 10, 2)
                + Fixed.date(openDate)
                + Fixed.date(expirationDate)
                + Fixed.date(reissueDate)
                + Zoned.formatSigned(currCycCredit, 10, 2)
                + Zoned.formatSigned(currCycDebit, 10, 2)
                + pad(addrZip, 10)
                + pad(groupId, 10)
                + " ".repeat(178);
    }
}
