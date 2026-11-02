package com.carddemo.account.client;

import java.time.LocalDate;

/**
 * customer-service {@code PUT /api/v1/customers/{customerId}} body: the CUST-UPDATE-RECORD COACTUPC
 * rewrites. {@code ssn} null keeps the stored SSN; {@code version} is the version the account view
 * was built from (409 when the customer changed since).
 */
public record CustomerUpdate(
        String firstName,
        String middleName,
        String lastName,
        String addressLine1,
        String addressLine2,
        String addressLine3,
        String stateCode,
        String countryCode,
        String zip,
        String phone1,
        String phone2,
        Long ssn,
        String governmentIssuedId,
        LocalDate dateOfBirth,
        String eftAccountId,
        String primaryCardHolderIndicator,
        Integer ficoScore,
        long version) {
}
