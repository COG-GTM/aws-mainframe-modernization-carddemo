package com.carddemo.programs.cbexport.copybook;

import java.util.List;

/**
 * CVEXPORT - 05 EXPORT-CUSTOMER-DATA REDEFINES EXPORT-RECORD-DATA (460 bytes).
 *
 * @param custId              EXP-CUST-ID PIC 9(09) COMP (4 bytes)
 * @param firstName           EXP-CUST-FIRST-NAME PIC X(25)
 * @param middleName          EXP-CUST-MIDDLE-NAME PIC X(25)
 * @param lastName            EXP-CUST-LAST-NAME PIC X(25)
 * @param addrLines           EXP-CUST-ADDR-LINE PIC X(50) OCCURS 3 TIMES
 * @param addrStateCd         EXP-CUST-ADDR-STATE-CD PIC X(02)
 * @param addrCountryCd       EXP-CUST-ADDR-COUNTRY-CD PIC X(03)
 * @param addrZip             EXP-CUST-ADDR-ZIP PIC X(10)
 * @param phoneNums           EXP-CUST-PHONE-NUM PIC X(15) OCCURS 2 TIMES
 * @param ssn                 EXP-CUST-SSN PIC 9(09)
 * @param govtIssuedId        EXP-CUST-GOVT-ISSUED-ID PIC X(20)
 * @param dobYyyyMmDd         EXP-CUST-DOB-YYYY-MM-DD PIC X(10)
 * @param eftAccountId        EXP-CUST-EFT-ACCOUNT-ID PIC X(10)
 * @param priCardHolderInd    EXP-CUST-PRI-CARD-HOLDER-IND PIC X(01)
 * @param ficoCreditScore     EXP-CUST-FICO-CREDIT-SCORE PIC 9(03) COMP-3 (2 bytes)
 */
public record ExportCustomerData(
        long custId,
        String firstName,
        String middleName,
        String lastName,
        List<String> addrLines,
        String addrStateCd,
        String addrCountryCd,
        String addrZip,
        List<String> phoneNums,
        long ssn,
        String govtIssuedId,
        String dobYyyyMmDd,
        String eftAccountId,
        String priCardHolderInd,
        int ficoCreditScore) {

    public static final int ADDR_LINE_OCCURS = 3;
    public static final int PHONE_NUM_OCCURS = 2;

    public ExportCustomerData {
        addrLines = List.copyOf(addrLines);
        phoneNums = List.copyOf(phoneNums);
        if (addrLines.size() != ADDR_LINE_OCCURS || phoneNums.size() != PHONE_NUM_OCCURS) {
            throw new IllegalArgumentException("OCCURS mismatch: addrLines=3, phoneNums=2 required");
        }
    }
}
