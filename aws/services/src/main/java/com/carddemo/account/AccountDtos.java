package com.carddemo.account;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.List;

public final class AccountDtos {

    private AccountDtos() {
    }

    public record AccountView(
            long acctId,
            String activeStatus,
            String currBal,
            String creditLimit,
            String cashCreditLimit,
            String openDate,
            String expirationDate,
            String reissueDate,
            String currCycCredit,
            String currCycDebit,
            String groupId,
            String addrZip,
            long version,
            CustomerView customer,
            List<CardRef> cards,
            @JsonInclude(JsonInclude.Include.NON_NULL) String message) {

        public AccountView withMessage(String newMessage) {
            return new AccountView(acctId, activeStatus, currBal, creditLimit, cashCreditLimit, openDate,
                    expirationDate, reissueDate, currCycCredit, currCycDebit, groupId, addrZip, version, customer,
                    cards, newMessage);
        }
    }

    public record CustomerView(
            int custId,
            String firstName,
            String middleName,
            String lastName,
            String addrLine1,
            String addrLine2,
            String addrLine3,
            String addrStateCd,
            String addrCountryCd,
            String addrZip,
            String phoneNum1,
            String phoneNum2,
            String ssn,
            String govtIssuedId,
            String dob,
            String eftAccountId,
            String priCardHolderInd,
            Integer ficoCreditScore,
            long version) {
    }

    public record CardRef(String cardNum, String activeStatus) {
    }

    /** PUT body. Values are strings so the legacy screen edits (and their messages) can be applied verbatim. */
    public record AccountUpdateRequest(
            String activeStatus,
            String currBal,
            String creditLimit,
            String cashCreditLimit,
            String openDate,
            String expirationDate,
            String reissueDate,
            String currCycCredit,
            String currCycDebit,
            String groupId,
            String addrZip,
            Long version,
            CustomerUpdateRequest customer) {
    }

    public record CustomerUpdateRequest(
            String firstName,
            String middleName,
            String lastName,
            String addrLine1,
            String addrLine2,
            String addrLine3,
            String addrStateCd,
            String addrCountryCd,
            String addrZip,
            String phoneNum1,
            String phoneNum2,
            String ssn,
            String govtIssuedId,
            String dob,
            String eftAccountId,
            String priCardHolderInd,
            String ficoCreditScore,
            Long version) {
    }
}
