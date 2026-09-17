package com.carddemo.customer.api.dto;

import com.carddemo.customer.domain.Customer;
import java.time.LocalDate;

/**
 * Customer fields of the COACTVW screen. The SSN is masked: the 3270 screen showed it in full,
 * the modern API does not return it.
 */
public record CustomerResponse(
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
        Integer ficoScore) {

    public static CustomerResponse from(Customer customer) {
        return new CustomerResponse(
                customer.getCustId(),
                customer.getFirstName(),
                customer.getMiddleName(),
                customer.getLastName(),
                customer.getAddrLine1(),
                customer.getAddrLine2(),
                customer.getAddrLine3(),
                customer.getAddrStateCd(),
                customer.getAddrCountryCd(),
                customer.getAddrZip(),
                customer.getPhoneNum1(),
                customer.getPhoneNum2(),
                last4(customer.getSsn()),
                customer.getGovtIssuedId(),
                customer.getDateOfBirth(),
                customer.getEftAccountId(),
                customer.getPriCardHolderInd(),
                customer.getFicoCreditScore());
    }

    private static String last4(Long ssn) {
        if (ssn == null) {
            return null;
        }
        String digits = "%09d".formatted(ssn);
        return digits.substring(digits.length() - 4);
    }
}
