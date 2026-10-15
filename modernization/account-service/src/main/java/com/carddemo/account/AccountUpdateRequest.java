package com.carddemo.account;

import com.carddemo.common.domain.Account;
import com.carddemo.common.domain.Customer;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Locale;

/** Editable fields of the CACTUPA map plus the record versions the client was shown. */
public record AccountUpdateRequest(
        String activeStatus, BigDecimal creditLimit, BigDecimal cashCreditLimit, BigDecimal currBal,
        BigDecimal currCycCredit, BigDecimal currCycDebit, LocalDate openDate, LocalDate expirationDate,
        LocalDate reissueDate, String groupId,
        String firstName, String middleName, String lastName, String addrLine1, String addrLine2, String addrLine3,
        String addrStateCd, String addrCountryCd, String addrZip, String phoneNum1, String phoneNum2, String ssn,
        String govtIssuedId, LocalDate dob, String eftAccountId, String priCardHolderInd, Integer ficoCreditScore,
        long accountVersion, long customerVersion) {

    static AccountUpdateRequest from(AccountDetails d) {
        return new AccountUpdateRequest(d.activeStatus(), d.creditLimit(), d.cashCreditLimit(), d.currBal(),
                d.currCycCredit(), d.currCycDebit(), d.openDate(), d.expirationDate(), d.reissueDate(), d.groupId(),
                d.firstName(), d.middleName(), d.lastName(), d.addrLine1(), d.addrLine2(), d.addrLine3(),
                d.addrStateCd(), d.addrCountryCd(), d.addrZip(), d.phoneNum1(), d.phoneNum2(), d.ssn(),
                d.govtIssuedId(), d.dob(), d.eftAccountId(), d.priCardHolderInd(), d.ficoCreditScore(),
                d.accountVersion(), d.customerVersion());
    }

    /** COACTUPC "no change detected" value: codes compare case-insensitively, free text exactly as it is stored. */
    String normalised() {
        return String.join("|", s(activeStatus), n(creditLimit), n(cashCreditLimit), n(currBal), n(currCycCredit),
                n(currCycDebit), String.valueOf(openDate), String.valueOf(expirationDate),
                String.valueOf(reissueDate), s(groupId), t(firstName), t(middleName), t(lastName), t(addrLine1),
                t(addrLine2), t(addrLine3), s(addrStateCd), s(addrCountryCd), t(addrZip), t(phoneNum1),
                t(phoneNum2), s(ssn), t(govtIssuedId), String.valueOf(dob), t(eftAccountId), s(priCardHolderInd),
                String.valueOf(ficoCreditScore));
    }

    void applyTo(Account a, Customer c) {
        a.setActiveStatus(activeStatus.toUpperCase(Locale.ROOT));
        a.setCreditLimit(creditLimit);
        a.setCashCreditLimit(cashCreditLimit);
        a.setCurrBal(currBal);
        a.setCurrCycCredit(currCycCredit);
        a.setCurrCycDebit(currCycDebit);
        a.setOpenDate(openDate);
        a.setExpirationDate(expirationDate);
        a.setReissueDate(reissueDate);
        a.setGroupId(blankToNull(groupId));
        c.setFirstName(firstName.trim());
        c.setMiddleName(blankToNull(middleName));
        c.setLastName(lastName.trim());
        c.setAddrLine1(addrLine1.trim());
        c.setAddrLine2(blankToNull(addrLine2));
        c.setAddrLine3(addrLine3.trim());
        c.setAddrStateCd(addrStateCd.toUpperCase(Locale.ROOT));
        c.setAddrCountryCd(addrCountryCd.toUpperCase(Locale.ROOT));
        c.setAddrZip(addrZip.trim());
        a.setAddrZip(addrZip.trim());
        c.setPhoneNum1(phoneNum1.trim());
        c.setPhoneNum2(blankToNull(phoneNum2));
        c.setSsn(ssn.replace("-", ""));
        c.setGovtIssuedId(blankToNull(govtIssuedId));
        c.setDob(dob);
        c.setEftAccountId(eftAccountId.trim());
        c.setPriCardHolderInd(priCardHolderInd.toUpperCase(Locale.ROOT));
        c.setFicoCreditScore(ficoCreditScore);
    }

    private static String t(String v) {
        return v == null ? "" : v.trim();
    }

    private static String s(String v) {
        return v == null ? "" : v.trim().toUpperCase(Locale.ROOT);
    }

    private static String n(BigDecimal v) {
        return v == null ? "" : v.stripTrailingZeros().toPlainString();
    }

    private static String blankToNull(String v) {
        return v == null || v.isBlank() ? null : v.trim();
    }
}
