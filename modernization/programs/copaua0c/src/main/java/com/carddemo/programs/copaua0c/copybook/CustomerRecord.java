package com.carddemo.programs.copaua0c.copybook;

import com.carddemo.programs.copaua0c.support.Cobol;

/** CVCUS01Y - 01 CUSTOMER-RECORD (RECLN 500; FILLER X(168) dropped). */
public record CustomerRecord(
        long custId,                 // CUST-ID                  PIC 9(09)
        String firstName,            // CUST-FIRST-NAME          PIC X(25)
        String middleName,           // CUST-MIDDLE-NAME         PIC X(25)
        String lastName,             // CUST-LAST-NAME           PIC X(25)
        String addrLine1,            // CUST-ADDR-LINE-1         PIC X(50)
        String addrLine2,            // CUST-ADDR-LINE-2         PIC X(50)
        String addrLine3,            // CUST-ADDR-LINE-3         PIC X(50)
        String addrStateCd,          // CUST-ADDR-STATE-CD       PIC X(02)
        String addrCountryCd,        // CUST-ADDR-COUNTRY-CD     PIC X(03)
        String addrZip,              // CUST-ADDR-ZIP            PIC X(10)
        String phoneNum1,            // CUST-PHONE-NUM-1         PIC X(15)
        String phoneNum2,            // CUST-PHONE-NUM-2         PIC X(15)
        long ssn,                    // CUST-SSN                 PIC 9(09)
        String govtIssuedId,         // CUST-GOVT-ISSUED-ID      PIC X(20)
        String dobYyyyMmDd,          // CUST-DOB-YYYY-MM-DD      PIC X(10)
        String eftAccountId,         // CUST-EFT-ACCOUNT-ID      PIC X(10)
        String priCardHolderInd,     // CUST-PRI-CARD-HOLDER-IND PIC X(01)
        int ficoCreditScore) {       // CUST-FICO-CREDIT-SCORE   PIC 9(03)

    public static final int RECORD_LENGTH = 500;

    public static CustomerRecord fromFixed(String line) {
        String r = Cobol.alpha(line, RECORD_LENGTH);
        return new CustomerRecord(
                Cobol.unsigned(r.substring(0, 9)),
                r.substring(9, 34),
                r.substring(34, 59),
                r.substring(59, 84),
                r.substring(84, 134),
                r.substring(134, 184),
                r.substring(184, 234),
                r.substring(234, 236),
                r.substring(236, 239),
                r.substring(239, 249),
                r.substring(249, 264),
                r.substring(264, 279),
                Cobol.unsigned(r.substring(279, 288)),
                r.substring(288, 308),
                r.substring(308, 318),
                r.substring(318, 328),
                r.substring(328, 329),
                (int) Cobol.unsigned(r.substring(329, 332)));
    }
}
