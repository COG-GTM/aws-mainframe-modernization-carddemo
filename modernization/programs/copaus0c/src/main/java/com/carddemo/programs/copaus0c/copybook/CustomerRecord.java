package com.carddemo.programs.copaus0c.copybook;

/** CVCUS01Y - CUSTOMER-RECORD (RECLN 500). FILLER X(168) dropped. */
public record CustomerRecord(
        long custId,                 // CUST-ID                  PIC 9(09)
        String custFirstName,        // CUST-FIRST-NAME          PIC X(25)
        String custMiddleName,       // CUST-MIDDLE-NAME         PIC X(25)
        String custLastName,         // CUST-LAST-NAME           PIC X(25)
        String custAddrLine1,        // CUST-ADDR-LINE-1         PIC X(50)
        String custAddrLine2,        // CUST-ADDR-LINE-2         PIC X(50)
        String custAddrLine3,        // CUST-ADDR-LINE-3         PIC X(50)
        String custAddrStateCd,      // CUST-ADDR-STATE-CD       PIC X(02)
        String custAddrCountryCd,    // CUST-ADDR-COUNTRY-CD     PIC X(03)
        String custAddrZip,          // CUST-ADDR-ZIP            PIC X(10)
        String custPhoneNum1,        // CUST-PHONE-NUM-1         PIC X(15)
        String custPhoneNum2,        // CUST-PHONE-NUM-2         PIC X(15)
        long custSsn,                // CUST-SSN                 PIC 9(09)
        String custGovtIssuedId,     // CUST-GOVT-ISSUED-ID      PIC X(20)
        String custDobYyyyMmDd,      // CUST-DOB-YYYY-MM-DD      PIC X(10)
        String custEftAccountId,     // CUST-EFT-ACCOUNT-ID      PIC X(10)
        String custPriCardHolderInd, // CUST-PRI-CARD-HOLDER-IND PIC X(01)
        int custFicoCreditScore) {   // CUST-FICO-CREDIT-SCORE   PIC 9(03)

    public static final int RECORD_LENGTH = 500;

    public static CustomerRecord parse(String line) {
        String r = String.format("%-" + RECORD_LENGTH + "s", line);
        return new CustomerRecord(
                Long.parseLong(r.substring(0, 9)),
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
                Long.parseLong(r.substring(279, 288)),
                r.substring(288, 308),
                r.substring(308, 318),
                r.substring(318, 328),
                r.substring(328, 329),
                Integer.parseInt(r.substring(329, 332)));
    }
}
