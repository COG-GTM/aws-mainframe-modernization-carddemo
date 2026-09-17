package com.carddemo.account.api.dto;

import com.carddemo.account.domain.Account;
import java.math.BigDecimal;
import java.time.LocalDate;

/** Screen fields of COACTVW, as JSON. */
public record AccountResponse(
        Long accountId,
        String activeStatus,
        BigDecimal currentBalance,
        BigDecimal creditLimit,
        BigDecimal cashCreditLimit,
        LocalDate openDate,
        LocalDate expirationDate,
        LocalDate reissueDate,
        BigDecimal currentCycleCredit,
        BigDecimal currentCycleDebit,
        String addressZip,
        String groupId) {

    public static AccountResponse from(Account account) {
        return new AccountResponse(
                account.getAcctId(),
                account.getActiveStatus(),
                account.getCurrBal(),
                account.getCreditLimit(),
                account.getCashCreditLimit(),
                account.getOpenDate(),
                account.getExpirationDate(),
                account.getReissueDate(),
                account.getCurrCycCredit(),
                account.getCurrCycDebit(),
                account.getAddrZip(),
                account.getGroupId());
    }
}
