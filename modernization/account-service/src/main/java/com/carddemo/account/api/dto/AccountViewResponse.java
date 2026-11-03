package com.carddemo.account.api.dto;

import com.carddemo.account.client.CustomerView;
import com.carddemo.account.domain.Account;
import com.carddemo.common.api.PicId;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * The output fields of the COACTVW map (CACTVWA) as filled by COACTVWC 1200-SETUP-SCREEN-VARS: the
 * account record, the customer found through the CXACAIX cross reference, plus the record versions a
 * client sends back on {@code PUT /api/v1/accounts/{accountId}} (COACTUPC). The SSN keeps the screen
 * layout {@code 999-99-9999} but is masked to the last four digits, as customer-service returns it.
 */
public record AccountViewResponse(
        @PicId(digits = 11) Long accountId,
        String activeStatus,
        LocalDate openDate,
        BigDecimal creditLimit,
        LocalDate expirationDate,
        BigDecimal cashCreditLimit,
        LocalDate reissueDate,
        BigDecimal currentBalance,
        BigDecimal currentCycleCredit,
        String groupId,
        BigDecimal currentCycleDebit,
        @PicId(digits = 9) Long customerId,
        String ssn,
        LocalDate dateOfBirth,
        Integer ficoScore,
        String firstName,
        String middleName,
        String lastName,
        String addressLine1,
        String addressLine2,
        String city,
        String stateCode,
        String zip,
        String countryCode,
        String phoneNumber1,
        String phoneNumber2,
        String governmentIssuedId,
        String eftAccountId,
        String primaryCardHolder,
        long accountVersion,
        long customerVersion) {

    public static AccountViewResponse from(Account account, CustomerView customer) {
        return new AccountViewResponse(
                account.getAcctId(),
                account.getActiveStatus(),
                account.getOpenDate(),
                account.getCreditLimit(),
                account.getExpirationDate(),
                account.getCashCreditLimit(),
                account.getReissueDate(),
                account.getCurrBal(),
                account.getCurrCycCredit(),
                account.getGroupId(),
                account.getCurrCycDebit(),
                customer.customerId(),
                customer.ssnLast4() == null ? null : "***-**-" + customer.ssnLast4(),
                customer.dateOfBirth(),
                customer.ficoScore(),
                customer.firstName(),
                customer.middleName(),
                customer.lastName(),
                customer.addressLine1(),
                customer.addressLine2(),
                customer.addressLine3(),
                customer.stateCode(),
                customer.zip(),
                customer.countryCode(),
                customer.phone1(),
                customer.phone2(),
                customer.governmentIssuedId(),
                customer.eftAccountId(),
                customer.primaryCardHolderIndicator(),
                account.getVersion(),
                customer.version());
    }
}
