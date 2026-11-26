package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/** CVCUS01Y: {@code CUSTOMER-RECORD} (RECLN 500). */
public record CustomerRecord(
        long custId,                // 9(09)
        String custFirstName,       // X(25)
        String custMiddleName,      // X(25)
        String custLastName,        // X(25)
        String custAddrLine1,       // X(50)
        String custAddrLine2,       // X(50)
        String custAddrLine3,       // X(50)
        String custAddrStateCd,     // X(02)
        String custAddrCountryCd,   // X(03)
        String custAddrZip,         // X(10)
        String custPhoneNum1,       // X(15)
        String custPhoneNum2,       // X(15)
        long custSsn,               // 9(09)
        String custGovtIssuedId,    // X(20)
        String custDobYyyyMmDd,     // X(10)
        String custEftAccountId,    // X(10)
        String custPriCardHolderInd, // X(01)
        int custFicoCreditScore) {  // 9(03); FILLER X(168) dropped
    public static final int LENGTH = 500;

    public static CustomerRecord parse(String line) {
        String s = Cobol.alnum(line, LENGTH);
        return new CustomerRecord(
                Cobol.toLong(s.substring(0, 9)),
                s.substring(9, 34), s.substring(34, 59), s.substring(59, 84),
                s.substring(84, 134), s.substring(134, 184), s.substring(184, 234),
                s.substring(234, 236), s.substring(236, 239), s.substring(239, 249),
                s.substring(249, 264), s.substring(264, 279),
                Cobol.toLong(s.substring(279, 288)),
                s.substring(288, 308), s.substring(308, 318), s.substring(318, 328), s.substring(328, 329),
                (int) Cobol.toLong(s.substring(329, 332)));
    }

    public String toFixedWidth() {
        return Cobol.digits(custId, 9) + Cobol.alnum(custFirstName, 25) + Cobol.alnum(custMiddleName, 25)
                + Cobol.alnum(custLastName, 25) + Cobol.alnum(custAddrLine1, 50) + Cobol.alnum(custAddrLine2, 50)
                + Cobol.alnum(custAddrLine3, 50) + Cobol.alnum(custAddrStateCd, 2) + Cobol.alnum(custAddrCountryCd, 3)
                + Cobol.alnum(custAddrZip, 10) + Cobol.alnum(custPhoneNum1, 15) + Cobol.alnum(custPhoneNum2, 15)
                + Cobol.digits(custSsn, 9) + Cobol.alnum(custGovtIssuedId, 20) + Cobol.alnum(custDobYyyyMmDd, 10)
                + Cobol.alnum(custEftAccountId, 10) + Cobol.alnum(custPriCardHolderInd, 1)
                + Cobol.digits(custFicoCreditScore, 3) + Cobol.spaces(168);
    }
}
