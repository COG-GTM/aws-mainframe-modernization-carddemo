package com.carddemo.customer;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;

/**
 * One CUSTDATA record (copybook CVCUS01Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link Customer} persists it.
 *
 * @param custId CUST-ID PIC 9(09)
 * @param firstName CUST-FIRST-NAME PIC X(25)
 * @param middleName CUST-MIDDLE-NAME PIC X(25)
 * @param lastName CUST-LAST-NAME PIC X(25)
 * @param addrLine1 CUST-ADDR-LINE-1 PIC X(50)
 * @param addrLine2 CUST-ADDR-LINE-2 PIC X(50)
 * @param addrLine3 CUST-ADDR-LINE-3 PIC X(50)
 * @param addrStateCd CUST-ADDR-STATE-CD PIC X(02)
 * @param addrCountryCd CUST-ADDR-COUNTRY-CD PIC X(03)
 * @param addrZip CUST-ADDR-ZIP PIC X(10)
 * @param phoneNum1 CUST-PHONE-NUM-1 PIC X(15)
 * @param phoneNum2 CUST-PHONE-NUM-2 PIC X(15)
 * @param ssn CUST-SSN PIC 9(09)
 * @param govtIssuedId CUST-GOVT-ISSUED-ID PIC X(20)
 * @param dob CUST-DOB-YYYY-MM-DD PIC X(10)
 * @param eftAccountId CUST-EFT-ACCOUNT-ID PIC X(10)
 * @param priCardHolderInd CUST-PRI-CARD-HOLDER-IND PIC X(01)
 * @param ficoCreditScore CUST-FICO-CREDIT-SCORE PIC 9(03)
 */
public record CustomerRecord(
        @CobolField("CUST-ID") int custId,
        @CobolField("CUST-FIRST-NAME") String firstName,
        @CobolField("CUST-MIDDLE-NAME") String middleName,
        @CobolField("CUST-LAST-NAME") String lastName,
        @CobolField("CUST-ADDR-LINE-1") String addrLine1,
        @CobolField("CUST-ADDR-LINE-2") String addrLine2,
        @CobolField("CUST-ADDR-LINE-3") String addrLine3,
        @CobolField("CUST-ADDR-STATE-CD") String addrStateCd,
        @CobolField("CUST-ADDR-COUNTRY-CD") String addrCountryCd,
        @CobolField("CUST-ADDR-ZIP") String addrZip,
        @CobolField("CUST-PHONE-NUM-1") String phoneNum1,
        @CobolField("CUST-PHONE-NUM-2") String phoneNum2,
        @CobolField("CUST-SSN") int ssn,
        @CobolField("CUST-GOVT-ISSUED-ID") String govtIssuedId,
        @CobolField("CUST-DOB-YYYY-MM-DD") String dob,
        @CobolField("CUST-EFT-ACCOUNT-ID") String eftAccountId,
        @CobolField("CUST-PRI-CARD-HOLDER-IND") PrimaryCardHolder priCardHolderInd,
        @CobolField("CUST-FICO-CREDIT-SCORE") int ficoCreditScore) {

    public static final CopybookRecordMapper<CustomerRecord> MAPPER =
            CopybookRecordMapper.of(CustomerRecord.class, "CVCUS01Y");
}
