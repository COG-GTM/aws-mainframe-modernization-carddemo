package com.carddemo.account;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.Customer;
import java.math.BigDecimal;
import java.time.LocalDate;

/** Fields of the CACTVWA / CACTUPA maps: account (CVACT01Y) joined to its customer (CVCUS01Y) via CXACAIX. */
public record AccountDetails(
        Long acctId, String activeStatus, BigDecimal currBal, BigDecimal creditLimit, BigDecimal cashCreditLimit,
        LocalDate openDate, LocalDate expirationDate, LocalDate reissueDate, BigDecimal currCycCredit,
        BigDecimal currCycDebit, String groupId,
        Long custId, String firstName, String middleName, String lastName, String addrLine1, String addrLine2,
        String addrLine3, String addrStateCd, String addrCountryCd, String addrZip, String phoneNum1,
        String phoneNum2, String ssn, String govtIssuedId, LocalDate dob, String eftAccountId,
        String priCardHolderInd, Integer ficoCreditScore, long accountVersion, long customerVersion) {

    static AccountDetails of(Account a, Customer c) {
        return new AccountDetails(a.getAcctId(), a.getActiveStatus(), a.getCurrBal(), a.getCreditLimit(),
                a.getCashCreditLimit(), a.getOpenDate(), a.getExpirationDate(), a.getReissueDate(),
                a.getCurrCycCredit(), a.getCurrCycDebit(), a.getGroupId(),
                c.getCustId(), c.getFirstName(), c.getMiddleName(), c.getLastName(), c.getAddrLine1(),
                c.getAddrLine2(), c.getAddrLine3(), c.getAddrStateCd(), c.getAddrCountryCd(), c.getAddrZip(),
                c.getPhoneNum1(), c.getPhoneNum2(), c.getSsn(), c.getGovtIssuedId(), c.getDob(),
                c.getEftAccountId(), c.getPriCardHolderInd(), c.getFicoCreditScore(), a.getVersion(),
                c.getVersion());
    }
}
