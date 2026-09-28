package com.carddemo.batch.record;

import static com.carddemo.batch.record.Fixed.field;
import static com.carddemo.batch.record.Fixed.text;

import java.time.LocalDate;

/** {@code CVCUS01Y}/{@code CUSTREC} CUSTOMER-RECORD (500 bytes). */
public record CustomerRecord(
        int custId,
        String firstName,
        String middleName,
        String lastName,
        String addrLine1,
        String addrLine2,
        String addrLine3,
        String addrStateCd,
        String addrCountryCd,
        String addrZip,
        String phoneNum1,
        String phoneNum2,
        String ssn,
        String govtIssuedId,
        LocalDate dob,
        String eftAccountId,
        String priCardHolderInd,
        Integer ficoCreditScore) {

    public static final int LENGTH = 500;

    public static CustomerRecord parse(String line) {
        String r = Fixed.normalize(line, LENGTH);
        String fico = field(r, 330, 3);
        return new CustomerRecord(
                (int) Zoned.parseUnsigned(field(r, 1, 9)),
                Fixed.rtrim(field(r, 10, 25)),
                text(field(r, 35, 25)),
                Fixed.rtrim(field(r, 60, 25)),
                text(field(r, 85, 50)),
                text(field(r, 135, 50)),
                text(field(r, 185, 50)),
                text(field(r, 235, 2)),
                text(field(r, 237, 3)),
                text(field(r, 240, 10)),
                text(field(r, 250, 15)),
                text(field(r, 265, 15)),
                field(r, 280, 9),
                text(field(r, 289, 20)),
                Fixed.date(field(r, 309, 10)),
                text(field(r, 319, 10)),
                text(field(r, 329, 1)),
                fico.isBlank() ? null : (int) Zoned.parseUnsigned(fico));
    }
}
