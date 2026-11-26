package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.FixedWidth;

/**
 * CVCUS01Y - CUSTOMER-RECORD (CUSTDAT VSAM KSDS, RECLN 500, key CUST-ID).
 *
 * @param custId              CUST-ID PIC 9(09)
 * @param firstName           CUST-FIRST-NAME PIC X(25)
 * @param middleName          CUST-MIDDLE-NAME PIC X(25)
 * @param lastName            CUST-LAST-NAME PIC X(25)
 * @param addrLine1           CUST-ADDR-LINE-1 PIC X(50)
 * @param addrLine2           CUST-ADDR-LINE-2 PIC X(50)
 * @param addrLine3           CUST-ADDR-LINE-3 PIC X(50)
 * @param addrStateCd         CUST-ADDR-STATE-CD PIC X(02)
 * @param addrCountryCd       CUST-ADDR-COUNTRY-CD PIC X(03)
 * @param addrZip             CUST-ADDR-ZIP PIC X(10)
 * @param phoneNum1           CUST-PHONE-NUM-1 PIC X(15)
 * @param phoneNum2           CUST-PHONE-NUM-2 PIC X(15)
 * @param ssn                 CUST-SSN PIC 9(09)
 * @param govtIssuedId        CUST-GOVT-ISSUED-ID PIC X(20)
 * @param dobYyyyMmDd         CUST-DOB-YYYY-MM-DD PIC X(10)
 * @param eftAccountId        CUST-EFT-ACCOUNT-ID PIC X(10)
 * @param priCardHolderInd    CUST-PRI-CARD-HOLDER-IND PIC X(01)
 * @param ficoCreditScore     CUST-FICO-CREDIT-SCORE PIC 9(03)
 * @param filler              FILLER PIC X(168)
 */
public record CustomerRecord(
        long custId,
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
        long ssn,
        String govtIssuedId,
        String dobYyyyMmDd,
        String eftAccountId,
        String priCardHolderInd,
        int ficoCreditScore,
        String filler) {

    public static final int RECORD_LENGTH = 500;

    public static CustomerRecord parse(String image) {
        FixedWidth f = new FixedWidth(image, RECORD_LENGTH);
        return new CustomerRecord(
                Cobol.unsignedValue(f.next(9)),
                f.next(25), f.next(25), f.next(25),
                f.next(50), f.next(50), f.next(50),
                f.next(2), f.next(3), f.next(10),
                f.next(15), f.next(15),
                Cobol.unsignedValue(f.next(9)),
                f.next(20), f.next(10), f.next(10), f.next(1),
                (int) Cobol.unsignedValue(f.next(3)),
                f.next(168));
    }

    public static CustomerRecord lowValues() {
        return parse(Cobol.lowValues(RECORD_LENGTH));
    }

    public String toFixedWidth() {
        return Cobol.unsignedDisplay(custId, 9)
                + Cobol.pad(firstName, 25) + Cobol.pad(middleName, 25) + Cobol.pad(lastName, 25)
                + Cobol.pad(addrLine1, 50) + Cobol.pad(addrLine2, 50) + Cobol.pad(addrLine3, 50)
                + Cobol.pad(addrStateCd, 2) + Cobol.pad(addrCountryCd, 3) + Cobol.pad(addrZip, 10)
                + Cobol.pad(phoneNum1, 15) + Cobol.pad(phoneNum2, 15)
                + Cobol.unsignedDisplay(ssn, 9)
                + Cobol.pad(govtIssuedId, 20) + Cobol.pad(dobYyyyMmDd, 10) + Cobol.pad(eftAccountId, 10)
                + Cobol.pad(priCardHolderInd, 1)
                + Cobol.unsignedDisplay(ficoCreditScore, 3)
                + Cobol.pad(filler, 168);
    }

    public String key() {
        return Cobol.unsignedDisplay(custId, 9);
    }
}
