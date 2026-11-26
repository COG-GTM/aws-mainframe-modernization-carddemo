package com.carddemo.programs.cbexport.copybook;

import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.getUnsignedZoned;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putAlnum;
import static com.carddemo.programs.cbexport.copybook.CobolFieldCodec.putUnsignedZoned;

/**
 * CVCUS01Y - 01 CUSTOMER-RECORD (RECLN 500). All fields USAGE DISPLAY.
 *
 * @param custId               CUST-ID PIC 9(09)
 * @param firstName            CUST-FIRST-NAME PIC X(25)
 * @param middleName           CUST-MIDDLE-NAME PIC X(25)
 * @param lastName             CUST-LAST-NAME PIC X(25)
 * @param addrLine1            CUST-ADDR-LINE-1 PIC X(50)
 * @param addrLine2            CUST-ADDR-LINE-2 PIC X(50)
 * @param addrLine3            CUST-ADDR-LINE-3 PIC X(50)
 * @param addrStateCd          CUST-ADDR-STATE-CD PIC X(02)
 * @param addrCountryCd        CUST-ADDR-COUNTRY-CD PIC X(03)
 * @param addrZip              CUST-ADDR-ZIP PIC X(10)
 * @param phoneNum1            CUST-PHONE-NUM-1 PIC X(15)
 * @param phoneNum2            CUST-PHONE-NUM-2 PIC X(15)
 * @param ssn                  CUST-SSN PIC 9(09)
 * @param govtIssuedId         CUST-GOVT-ISSUED-ID PIC X(20)
 * @param dobYyyyMmDd          CUST-DOB-YYYY-MM-DD PIC X(10)
 * @param eftAccountId         CUST-EFT-ACCOUNT-ID PIC X(10)
 * @param priCardHolderInd     CUST-PRI-CARD-HOLDER-IND PIC X(01)
 * @param ficoCreditScore      CUST-FICO-CREDIT-SCORE PIC 9(03)
 * @param filler               FILLER PIC X(168)
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

    public static final int LENGTH = 500;

    public static CustomerRecord parse(byte[] r) {
        return new CustomerRecord(
                getUnsignedZoned(r, 0, 9),
                getAlnum(r, 9, 25),
                getAlnum(r, 34, 25),
                getAlnum(r, 59, 25),
                getAlnum(r, 84, 50),
                getAlnum(r, 134, 50),
                getAlnum(r, 184, 50),
                getAlnum(r, 234, 2),
                getAlnum(r, 236, 3),
                getAlnum(r, 239, 10),
                getAlnum(r, 249, 15),
                getAlnum(r, 264, 15),
                getUnsignedZoned(r, 279, 9),
                getAlnum(r, 288, 20),
                getAlnum(r, 308, 10),
                getAlnum(r, 318, 10),
                getAlnum(r, 328, 1),
                (int) getUnsignedZoned(r, 329, 3),
                getAlnum(r, 332, 168));
    }

    public byte[] toBytes() {
        byte[] r = new byte[LENGTH];
        putUnsignedZoned(r, 0, 9, custId);
        putAlnum(r, 9, 25, firstName);
        putAlnum(r, 34, 25, middleName);
        putAlnum(r, 59, 25, lastName);
        putAlnum(r, 84, 50, addrLine1);
        putAlnum(r, 134, 50, addrLine2);
        putAlnum(r, 184, 50, addrLine3);
        putAlnum(r, 234, 2, addrStateCd);
        putAlnum(r, 236, 3, addrCountryCd);
        putAlnum(r, 239, 10, addrZip);
        putAlnum(r, 249, 15, phoneNum1);
        putAlnum(r, 264, 15, phoneNum2);
        putUnsignedZoned(r, 279, 9, ssn);
        putAlnum(r, 288, 20, govtIssuedId);
        putAlnum(r, 308, 10, dobYyyyMmDd);
        putAlnum(r, 318, 10, eftAccountId);
        putAlnum(r, 328, 1, priCardHolderInd);
        putUnsignedZoned(r, 329, 3, ficoCreditScore);
        putAlnum(r, 332, 168, filler);
        return r;
    }
}
