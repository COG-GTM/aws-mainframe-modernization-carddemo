package com.carddemo.programs.cbstm03a.copybook;

import com.carddemo.programs.cbstm03a.support.Pic;

/** CUSTREC - CUSTOMER-RECORD (RECLN 500). */
public record CustomerRecord(
        long custId,                  // CUST-ID                  PIC 9(09)
        String custFirstName,         // CUST-FIRST-NAME          PIC X(25)
        String custMiddleName,        // CUST-MIDDLE-NAME         PIC X(25)
        String custLastName,          // CUST-LAST-NAME           PIC X(25)
        String custAddrLine1,         // CUST-ADDR-LINE-1         PIC X(50)
        String custAddrLine2,         // CUST-ADDR-LINE-2         PIC X(50)
        String custAddrLine3,         // CUST-ADDR-LINE-3         PIC X(50)
        String custAddrStateCd,       // CUST-ADDR-STATE-CD       PIC X(02)
        String custAddrCountryCd,     // CUST-ADDR-COUNTRY-CD     PIC X(03)
        String custAddrZip,           // CUST-ADDR-ZIP            PIC X(10)
        String custPhoneNum1,         // CUST-PHONE-NUM-1         PIC X(15)
        String custPhoneNum2,         // CUST-PHONE-NUM-2         PIC X(15)
        long custSsn,                 // CUST-SSN                 PIC 9(09)
        String custGovtIssuedId,      // CUST-GOVT-ISSUED-ID      PIC X(20)
        String custDobYyyymmdd,       // CUST-DOB-YYYYMMDD        PIC X(10)
        String custEftAccountId,      // CUST-EFT-ACCOUNT-ID      PIC X(10)
        String custPriCardHolderInd,  // CUST-PRI-CARD-HOLDER-IND PIC X(01)
        int custFicoCreditScore,      // CUST-FICO-CREDIT-SCORE   PIC 9(03)
        String filler) {              // FILLER                   PIC X(168)

    public static final int LENGTH = 500;
    public static final int KEY_LENGTH = 9;

    public static CustomerRecord parse(String raw) {
        String r = Pic.x(raw, LENGTH);
        return new CustomerRecord(
                Pic.unsigned(r.substring(0, 9)),
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
                Pic.unsigned(r.substring(279, 288)),
                r.substring(288, 308),
                r.substring(308, 318),
                r.substring(318, 328),
                r.substring(328, 329),
                (int) Pic.unsigned(r.substring(329, 332)),
                r.substring(332, 500));
    }

    /** CUST-FICO-CREDIT-SCORE as its 3 DISPLAY digits. */
    public String custFicoCreditScoreDisplay() {
        return Pic.unsigned(custFicoCreditScore, 3);
    }

    public String toFixedWidth() {
        return Pic.unsigned(custId, 9)
                + Pic.x(custFirstName, 25)
                + Pic.x(custMiddleName, 25)
                + Pic.x(custLastName, 25)
                + Pic.x(custAddrLine1, 50)
                + Pic.x(custAddrLine2, 50)
                + Pic.x(custAddrLine3, 50)
                + Pic.x(custAddrStateCd, 2)
                + Pic.x(custAddrCountryCd, 3)
                + Pic.x(custAddrZip, 10)
                + Pic.x(custPhoneNum1, 15)
                + Pic.x(custPhoneNum2, 15)
                + Pic.unsigned(custSsn, 9)
                + Pic.x(custGovtIssuedId, 20)
                + Pic.x(custDobYyyymmdd, 10)
                + Pic.x(custEftAccountId, 10)
                + Pic.x(custPriCardHolderInd, 1)
                + custFicoCreditScoreDisplay()
                + Pic.x(filler, 168);
    }
}
