package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.displayValue;
import static com.carddemo.programs.cocrdupc.Cobol.numeric;

/** CVCUS01Y: 01 CUSTOMER-RECORD (500 bytes). COPYed by COCRDUPC but not referenced. */
public record CustomerRecord(
        long custId,                 // 9(09)
        String custFirstName,        // X(25)
        String custMiddleName,       // X(25)
        String custLastName,         // X(25)
        String custAddrLine1,        // X(50)
        String custAddrLine2,        // X(50)
        String custAddrLine3,        // X(50)
        String custAddrStateCd,      // X(02)
        String custAddrCountryCd,    // X(03)
        String custAddrZip,          // X(10)
        String custPhoneNum1,        // X(15)
        String custPhoneNum2,        // X(15)
        long custSsn,                // 9(09)
        String custGovtIssuedId,     // X(20)
        String custDobYyyyMmDd,      // X(10)
        String custEftAccountId,     // X(10)
        String custPriCardHolderInd, // X(01)
        int custFicoCreditScore,     // 9(03)
        String filler                // X(168)
) {
    public static final int LENGTH = 500;

    public CustomerRecord {
        custId = displayValue(numeric(custId, 9));
        custFirstName = alnum(custFirstName, 25);
        custMiddleName = alnum(custMiddleName, 25);
        custLastName = alnum(custLastName, 25);
        custAddrLine1 = alnum(custAddrLine1, 50);
        custAddrLine2 = alnum(custAddrLine2, 50);
        custAddrLine3 = alnum(custAddrLine3, 50);
        custAddrStateCd = alnum(custAddrStateCd, 2);
        custAddrCountryCd = alnum(custAddrCountryCd, 3);
        custAddrZip = alnum(custAddrZip, 10);
        custPhoneNum1 = alnum(custPhoneNum1, 15);
        custPhoneNum2 = alnum(custPhoneNum2, 15);
        custSsn = displayValue(numeric(custSsn, 9));
        custGovtIssuedId = alnum(custGovtIssuedId, 20);
        custDobYyyyMmDd = alnum(custDobYyyyMmDd, 10);
        custEftAccountId = alnum(custEftAccountId, 10);
        custPriCardHolderInd = alnum(custPriCardHolderInd, 1);
        custFicoCreditScore = (int) displayValue(numeric(custFicoCreditScore, 3));
        filler = alnum(filler, 168);
    }

    public static CustomerRecord parse(String raw) {
        String r = alnum(raw, LENGTH);
        return new CustomerRecord(displayValue(r.substring(0, 9)), r.substring(9, 34),
                r.substring(34, 59), r.substring(59, 84), r.substring(84, 134), r.substring(134, 184),
                r.substring(184, 234), r.substring(234, 236), r.substring(236, 239),
                r.substring(239, 249), r.substring(249, 264), r.substring(264, 279),
                displayValue(r.substring(279, 288)), r.substring(288, 308), r.substring(308, 318),
                r.substring(318, 328), r.substring(328, 329), (int) displayValue(r.substring(329, 332)),
                r.substring(332, 500));
    }

    public String toFixedWidth() {
        return numeric(custId, 9) + custFirstName + custMiddleName + custLastName + custAddrLine1
                + custAddrLine2 + custAddrLine3 + custAddrStateCd + custAddrCountryCd + custAddrZip
                + custPhoneNum1 + custPhoneNum2 + numeric(custSsn, 9) + custGovtIssuedId
                + custDobYyyyMmDd + custEftAccountId + custPriCardHolderInd
                + numeric(custFicoCreditScore, 3) + filler;
    }
}
