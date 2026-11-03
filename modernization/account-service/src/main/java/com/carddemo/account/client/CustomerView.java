package com.carddemo.account.client;

import java.time.LocalDate;

/** customer-service {@code GET /api/v1/customers/{customerId}}: the CUSTDAT record (SSN masked). */
public record CustomerView(
        Long customerId,
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
        String ssnLast4,
        String governmentIssuedId,
        LocalDate dateOfBirth,
        String eftAccountId,
        String primaryCardHolderIndicator,
        Integer ficoScore,
        long version) {
}
