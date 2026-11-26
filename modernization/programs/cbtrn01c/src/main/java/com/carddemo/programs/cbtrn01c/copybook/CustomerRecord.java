package com.carddemo.programs.cbtrn01c.copybook;

/** Copybook CVCUS01Y: 01 CUSTOMER-RECORD (RECLN 500). */
public record CustomerRecord(
        long custId,                // CUST-ID                  PIC 9(09)
        String firstName,           // CUST-FIRST-NAME          PIC X(25)
        String middleName,          // CUST-MIDDLE-NAME         PIC X(25)
        String lastName,            // CUST-LAST-NAME           PIC X(25)
        String addrLine1,           // CUST-ADDR-LINE-1         PIC X(50)
        String addrLine2,           // CUST-ADDR-LINE-2         PIC X(50)
        String addrLine3,           // CUST-ADDR-LINE-3         PIC X(50)
        String addrStateCd,         // CUST-ADDR-STATE-CD       PIC X(02)
        String addrCountryCd,       // CUST-ADDR-COUNTRY-CD     PIC X(03)
        String addrZip,             // CUST-ADDR-ZIP            PIC X(10)
        String phoneNum1,           // CUST-PHONE-NUM-1         PIC X(15)
        String phoneNum2,           // CUST-PHONE-NUM-2         PIC X(15)
        long ssn,                   // CUST-SSN                 PIC 9(09)
        String govtIssuedId,        // CUST-GOVT-ISSUED-ID      PIC X(20)
        String dobYyyyMmDd,         // CUST-DOB-YYYY-MM-DD      PIC X(10)
        String eftAccountId,        // CUST-EFT-ACCOUNT-ID      PIC X(10)
        String priCardHolderInd,    // CUST-PRI-CARD-HOLDER-IND PIC X(01)
        long ficoCreditScore,       // CUST-FICO-CREDIT-SCORE   PIC 9(03)
        String filler) {            // FILLER                   PIC X(168)

    public static final int LENGTH = 500;

    public CustomerRecord {
        custId = Pic.unsigned(custId, 9);
        firstName = Pic.alpha(firstName, 25);
        middleName = Pic.alpha(middleName, 25);
        lastName = Pic.alpha(lastName, 25);
        addrLine1 = Pic.alpha(addrLine1, 50);
        addrLine2 = Pic.alpha(addrLine2, 50);
        addrLine3 = Pic.alpha(addrLine3, 50);
        addrStateCd = Pic.alpha(addrStateCd, 2);
        addrCountryCd = Pic.alpha(addrCountryCd, 3);
        addrZip = Pic.alpha(addrZip, 10);
        phoneNum1 = Pic.alpha(phoneNum1, 15);
        phoneNum2 = Pic.alpha(phoneNum2, 15);
        ssn = Pic.unsigned(ssn, 9);
        govtIssuedId = Pic.alpha(govtIssuedId, 20);
        dobYyyyMmDd = Pic.alpha(dobYyyyMmDd, 10);
        eftAccountId = Pic.alpha(eftAccountId, 10);
        priCardHolderInd = Pic.alpha(priCardHolderInd, 1);
        ficoCreditScore = Pic.unsigned(ficoCreditScore, 3);
        filler = Pic.alpha(filler, 168);
    }

    public static CustomerRecord initial() {
        return new CustomerRecord(0, "", "", "", "", "", "", "", "", "", "", "", 0, "", "", "", "", 0, "");
    }

    public static final RecordCodec<CustomerRecord> CODEC = new RecordCodec<>() {
        @Override
        public int length() {
            return LENGTH;
        }

        @Override
        public CustomerRecord parse(String image) {
            Pic.Reader r = new Pic.Reader(image, LENGTH);
            return new CustomerRecord(r.n(9), r.x(25), r.x(25), r.x(25), r.x(50), r.x(50), r.x(50), r.x(2),
                    r.x(3), r.x(10), r.x(15), r.x(15), r.n(9), r.x(20), r.x(10), r.x(10), r.x(1), r.n(3),
                    r.x(168));
        }

        @Override
        public String format(CustomerRecord c) {
            return Pic.formatUnsigned(c.custId, 9) + c.firstName + c.middleName + c.lastName + c.addrLine1
                    + c.addrLine2 + c.addrLine3 + c.addrStateCd + c.addrCountryCd + c.addrZip + c.phoneNum1
                    + c.phoneNum2 + Pic.formatUnsigned(c.ssn, 9) + c.govtIssuedId + c.dobYyyyMmDd
                    + c.eftAccountId + c.priCardHolderInd + Pic.formatUnsigned(c.ficoCreditScore, 3) + c.filler;
        }
    };

    public String toFixedWidth() {
        return CODEC.format(this);
    }
}
