package com.carddemo.batch.model;

import com.carddemo.batch.io.FieldWriter;
import com.carddemo.batch.io.FixedWidthCodec;

/** {@code CUSTOMER-RECORD} from copybook CUSTREC (500 bytes), CUSTDATA KSDS keyed by {@code CUST-ID}. */
public record CustomerRecord(
        long custId,                   // PIC 9(09)
        String custFirstName,          // PIC X(25)
        String custMiddleName,         // PIC X(25)
        String custLastName,           // PIC X(25)
        String custAddrLine1,          // PIC X(50)
        String custAddrLine2,          // PIC X(50)
        String custAddrLine3,          // PIC X(50)
        String custAddrStateCd,        // PIC X(02)
        String custAddrCountryCd,      // PIC X(03)
        String custAddrZip,            // PIC X(10)
        String custPhoneNum1,          // PIC X(15)
        String custPhoneNum2,          // PIC X(15)
        long custSsn,                  // PIC 9(09)
        String custGovtIssuedId,       // PIC X(20)
        String custDobYyyymmdd,        // PIC X(10)
        String custEftAccountId,       // PIC X(10)
        String custPriCardHolderInd,   // PIC X(01)
        int custFicoCreditScore,       // PIC 9(03)
        String filler) {               // PIC X(168)

    public static final int LENGTH = 500;

    public static final FixedWidthCodec<CustomerRecord> CODEC = FixedWidthCodec.of(LENGTH,
            r -> new CustomerRecord(r.unsignedLong(9), r.text(25), r.text(25), r.text(25), r.text(50), r.text(50),
                    r.text(50), r.text(2), r.text(3), r.text(10), r.text(15), r.text(15), r.unsignedLong(9),
                    r.text(20), r.text(10), r.text(10), r.text(1), r.unsignedInt(3), r.text(168)),
            c -> new FieldWriter(LENGTH)
                    .unsigned(c.custId, 9)
                    .text(c.custFirstName, 25)
                    .text(c.custMiddleName, 25)
                    .text(c.custLastName, 25)
                    .text(c.custAddrLine1, 50)
                    .text(c.custAddrLine2, 50)
                    .text(c.custAddrLine3, 50)
                    .text(c.custAddrStateCd, 2)
                    .text(c.custAddrCountryCd, 3)
                    .text(c.custAddrZip, 10)
                    .text(c.custPhoneNum1, 15)
                    .text(c.custPhoneNum2, 15)
                    .unsigned(c.custSsn, 9)
                    .text(c.custGovtIssuedId, 20)
                    .text(c.custDobYyyymmdd, 10)
                    .text(c.custEftAccountId, 10)
                    .text(c.custPriCardHolderInd, 1)
                    .unsigned(c.custFicoCreditScore, 3)
                    .text(c.filler, 168)
                    .toString());
}
