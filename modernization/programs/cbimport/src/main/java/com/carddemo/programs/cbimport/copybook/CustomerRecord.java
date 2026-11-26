package com.carddemo.programs.cbimport.copybook;

/**
 * CVCUS01Y: CUSTOMER-RECORD, RECLN 500 (CUSTOUT, LRECL=500). Fields are normalised with COBOL MOVE
 * semantics on construction; FILLER X(168) is written as spaces.
 */
public record CustomerRecord(
        long custId,                    // CUST-ID 9(09)
        String firstName,               // CUST-FIRST-NAME X(25)
        String middleName,              // CUST-MIDDLE-NAME X(25)
        String lastName,                // CUST-LAST-NAME X(25)
        String addrLine1,               // CUST-ADDR-LINE-1 X(50)
        String addrLine2,               // CUST-ADDR-LINE-2 X(50)
        String addrLine3,               // CUST-ADDR-LINE-3 X(50)
        String addrStateCd,             // CUST-ADDR-STATE-CD X(02)
        String addrCountryCd,           // CUST-ADDR-COUNTRY-CD X(03)
        String addrZip,                 // CUST-ADDR-ZIP X(10)
        String phoneNum1,               // CUST-PHONE-NUM-1 X(15)
        String phoneNum2,               // CUST-PHONE-NUM-2 X(15)
        long ssn,                       // CUST-SSN 9(09)
        String govtIssuedId,            // CUST-GOVT-ISSUED-ID X(20)
        String dobYyyyMmDd,             // CUST-DOB-YYYY-MM-DD X(10)
        String eftAccountId,            // CUST-EFT-ACCOUNT-ID X(10)
        String priCardHolderInd,        // CUST-PRI-CARD-HOLDER-IND X(01)
        int ficoCreditScore) {          // CUST-FICO-CREDIT-SCORE 9(03)

    public static final int LENGTH = 500;
    public static final int FILLER_LENGTH = 168;

    public CustomerRecord {
        custId = Pic.unsigned(custId, 9);
        firstName = Pic.alnum(firstName, 25);
        middleName = Pic.alnum(middleName, 25);
        lastName = Pic.alnum(lastName, 25);
        addrLine1 = Pic.alnum(addrLine1, 50);
        addrLine2 = Pic.alnum(addrLine2, 50);
        addrLine3 = Pic.alnum(addrLine3, 50);
        addrStateCd = Pic.alnum(addrStateCd, 2);
        addrCountryCd = Pic.alnum(addrCountryCd, 3);
        addrZip = Pic.alnum(addrZip, 10);
        phoneNum1 = Pic.alnum(phoneNum1, 15);
        phoneNum2 = Pic.alnum(phoneNum2, 15);
        ssn = Pic.unsigned(ssn, 9);
        govtIssuedId = Pic.alnum(govtIssuedId, 20);
        dobYyyyMmDd = Pic.alnum(dobYyyyMmDd, 10);
        eftAccountId = Pic.alnum(eftAccountId, 10);
        priCardHolderInd = Pic.alnum(priCardHolderInd, 1);
        ficoCreditScore = (int) Pic.unsigned(ficoCreditScore, 3);
    }

    /** INITIALIZE CUSTOMER-RECORD: alphanumerics to spaces, numerics to zero. */
    public static CustomerRecord initialized() {
        return new CustomerRecord(0, "", "", "", "", "", "", "", "", "", "", "", 0, "", "", "", "", 0);
    }

    public String toFixedWidth() {
        return Pic.displayUnsigned(custId, 9) + firstName + middleName + lastName
                + addrLine1 + addrLine2 + addrLine3 + addrStateCd + addrCountryCd + addrZip
                + phoneNum1 + phoneNum2 + Pic.displayUnsigned(ssn, 9) + govtIssuedId + dobYyyyMmDd
                + eftAccountId + priCardHolderInd + Pic.displayUnsigned(ficoCreditScore, 3)
                + " ".repeat(FILLER_LENGTH);
    }

    public static CustomerRecord parse(String line) {
        Slicer s = new Slicer(line, LENGTH);
        return new CustomerRecord(s.unsigned(9), s.x(25), s.x(25), s.x(25), s.x(50), s.x(50), s.x(50),
                s.x(2), s.x(3), s.x(10), s.x(15), s.x(15), s.unsigned(9), s.x(20), s.x(10), s.x(10),
                s.x(1), (int) s.unsigned(3));
    }
}
