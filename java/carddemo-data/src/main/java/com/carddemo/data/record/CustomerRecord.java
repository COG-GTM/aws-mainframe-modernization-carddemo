package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

/** Fixed-width CUSTOMER-RECORD (flat: the address is grouped only in the entity). Layout: copybook CVCUS01Y. */
public record CustomerRecord(
        @CobolField("CUST-ID") Long id,
        @CobolField("CUST-FIRST-NAME") String firstName,
        @CobolField("CUST-MIDDLE-NAME") String middleName,
        @CobolField("CUST-LAST-NAME") String lastName,
        @CobolField("CUST-ADDR-LINE-1") String addressLine1,
        @CobolField("CUST-ADDR-LINE-2") String addressLine2,
        @CobolField("CUST-ADDR-LINE-3") String addressLine3,
        @CobolField("CUST-ADDR-STATE-CD") String addressStateCode,
        @CobolField("CUST-ADDR-COUNTRY-CD") String addressCountryCode,
        @CobolField("CUST-ADDR-ZIP") String addressZip,
        @CobolField("CUST-PHONE-NUM-1") String phoneNumber1,
        @CobolField("CUST-PHONE-NUM-2") String phoneNumber2,
        @CobolField("CUST-SSN") String ssn,
        @CobolField("CUST-GOVT-ISSUED-ID") String governmentIssuedId,
        @CobolField("CUST-DOB-YYYY-MM-DD") String dateOfBirth,
        @CobolField("CUST-EFT-ACCOUNT-ID") String eftAccountId,
        @CobolField("CUST-PRI-CARD-HOLDER-IND") String primaryCardHolderIndicator,
        @CobolField("CUST-FICO-CREDIT-SCORE") Integer ficoCreditScore) {
}
