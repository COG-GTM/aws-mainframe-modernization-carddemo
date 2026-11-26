package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;
import com.carddemo.programs.cocrdslc.FixedWidthReader;

/**
 * CVCUS01Y - CUSTOMER-RECORD (RECLN 500). COPYed by COCRDSLC but not referenced by its logic.
 */
public record CustomerRecord(
        long custId,
        String custFirstName,
        String custMiddleName,
        String custLastName,
        String custAddrLine1,
        String custAddrLine2,
        String custAddrLine3,
        String custAddrStateCd,
        String custAddrCountryCd,
        String custAddrZip,
        String custPhoneNum1,
        String custPhoneNum2,
        long custSsn,
        String custGovtIssuedId,
        String custDobYyyyMmDd,
        String custEftAccountId,
        String custPriCardHolderInd,
        int custFicoCreditScore) {

    public static final int RECORD_LENGTH = 500;
    public static final int FILLER_LEN = 168;

    public CustomerRecord {
        custFirstName = CobolText.pic(custFirstName, 25);
        custMiddleName = CobolText.pic(custMiddleName, 25);
        custLastName = CobolText.pic(custLastName, 25);
        custAddrLine1 = CobolText.pic(custAddrLine1, 50);
        custAddrLine2 = CobolText.pic(custAddrLine2, 50);
        custAddrLine3 = CobolText.pic(custAddrLine3, 50);
        custAddrStateCd = CobolText.pic(custAddrStateCd, 2);
        custAddrCountryCd = CobolText.pic(custAddrCountryCd, 3);
        custAddrZip = CobolText.pic(custAddrZip, 10);
        custPhoneNum1 = CobolText.pic(custPhoneNum1, 15);
        custPhoneNum2 = CobolText.pic(custPhoneNum2, 15);
        custGovtIssuedId = CobolText.pic(custGovtIssuedId, 20);
        custDobYyyyMmDd = CobolText.pic(custDobYyyyMmDd, 10);
        custEftAccountId = CobolText.pic(custEftAccountId, 10);
        custPriCardHolderInd = CobolText.pic(custPriCardHolderInd, 1);
    }

    public static CustomerRecord parse(String line) {
        FixedWidthReader r = new FixedWidthReader(line, RECORD_LENGTH);
        return new CustomerRecord(r.n(9), r.x(25), r.x(25), r.x(25), r.x(50), r.x(50), r.x(50),
                r.x(2), r.x(3), r.x(10), r.x(15), r.x(15), r.n(9), r.x(20), r.x(10), r.x(10),
                r.x(1), (int) r.n(3));
    }

    public String toFixedWidth() {
        return CobolText.zoned(custId, 9) + custFirstName + custMiddleName + custLastName
                + custAddrLine1 + custAddrLine2 + custAddrLine3 + custAddrStateCd + custAddrCountryCd
                + custAddrZip + custPhoneNum1 + custPhoneNum2 + CobolText.zoned(custSsn, 9)
                + custGovtIssuedId + custDobYyyyMmDd + custEftAccountId + custPriCardHolderInd
                + CobolText.zoned(custFicoCreditScore, 3) + CobolText.spaces(FILLER_LEN);
    }
}
