package com.carddemo.web.account;

import com.carddemo.account.Account;
import com.carddemo.account.online.AccountChanges;
import com.carddemo.account.online.AccountDetails;
import com.carddemo.customer.Customer;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;
import java.util.List;

/**
 * Map {@code CACTVWA} (mapset {@code COACTVW}) after {@code 9000-READ-ACCT}: every output field of the map (BMS name in
 * each description), the cards on the account, the versions to send back on update, and the update form prefilled
 * like {@code COACTUPC} shows it.
 */
@Schema(description = "CACTVWA account view: account + customer + card numbers")
public record AccountViewScreen(
        ScreenHeader header,
        @Schema(description = "INFOMSG", example = "Enter or update id of account to display") String infoMessage,
        @Schema(description = "ERRMSG (empty after a successful read)", example = "") String message,
        @Schema(description = "ACCTSID: account id, 11 digits", example = "00000000001") String acctId,
        @Schema(description = "ACSTTUS: active status Y/N", example = "Y") String activeStatus,
        @Schema(description = "ADTOPEN: open date", example = "2014-11-20") String openDate,
        @Schema(description = "AEXPDT: expiration date", example = "2025-05-20") String expirationDate,
        @Schema(description = "AREISDT: reissue date", example = "2025-05-20") String reissueDate,
        @Schema(description = "ACRDLIM: credit limit", example = "20200.00") BigDecimal creditLimit,
        @Schema(description = "ACSHLIM: cash credit limit", example = "10200.00") BigDecimal cashCreditLimit,
        @Schema(description = "ACURBAL: current balance", example = "1940.00") BigDecimal currentBalance,
        @Schema(description = "ACRCYCR: current cycle credit", example = "0.00") BigDecimal currentCycleCredit,
        @Schema(description = "ACRCYDB: current cycle debit", example = "0.00") BigDecimal currentCycleDebit,
        @Schema(description = "AADDGRP: account group", example = "A000000000") String groupId,
        @Schema(description = "ACSTNUM: customer id, 9 digits", example = "000000001") String custId,
        @Schema(description = "ACSTSSN: SSN as 999-99-9999", example = "020-97-3888") String ssn,
        @Schema(description = "ACSTFCO: FICO score", example = "274") String ficoScore,
        @Schema(description = "ACSTDOB: date of birth", example = "1961-06-08") String dateOfBirth,
        @Schema(description = "ACSFNAM", example = "Immanuel") String firstName,
        @Schema(description = "ACSMNAM", example = "Madeline") String middleName,
        @Schema(description = "ACSLNAM", example = "Kessler") String lastName,
        @Schema(description = "ACSADL1", example = "618 Deshaun Route") String addressLine1,
        @Schema(description = "ACSADL2", example = "Apt. 802") String addressLine2,
        @Schema(description = "ACSCITY (address line 3)", example = "Altenwerthshire") String city,
        @Schema(description = "ACSSTTE", example = "NC") String state,
        @Schema(description = "ACSZIPC", example = "12546") String zip,
        @Schema(description = "ACSCTRY", example = "USA") String country,
        @Schema(description = "ACSPHN1", example = "(908)119-8310") String phone1,
        @Schema(description = "ACSPHN2", example = "(373)693-8684") String phone2,
        @Schema(description = "ACSGOVT", example = "00000000000049368437") String governmentId,
        @Schema(description = "ACSEFTC", example = "0053581756") String eftAccountId,
        @Schema(description = "ACSPFLG: primary card holder Y/N", example = "Y") String primaryCardHolder,
        @Schema(description = "CDEMO-CARD-NUM: card of the CXACAIX record read", example = "0500024453765740")
        String cardNum,
        @Schema(description = "All card numbers of the account (CXACAIX), ascending") List<String> cardNumbers,
        @Schema(description = "Optimistic-lock version of the account", example = "0") long accountVersion,
        @Schema(description = "Optimistic-lock version of the customer", example = "0") long customerVersion,
        @Schema(description = "PUT /api/v1/accounts/{id} body prefilled with the stored values (COACTUPC "
                + "3202-SHOW-ORIGINAL-VALUES); change fields and send it back") AccountUpdateRequest updateForm,
        @Schema(description = "PF3: navigation back to the calling program (default main menu)")
        NavigationContext exit) {

    static AccountViewScreen of(ScreenHeader header, String infoMessage, String message, AccountDetails details,
            NavigationContext exit) {
        Account a = details.account();
        Customer c = details.customer();
        String ssn = String.format("%09d", c.getSsn());
        return new AccountViewScreen(header, infoMessage, message, String.format("%011d", a.getAcctId()),
                a.getActiveStatus() == null ? "" : a.getActiveStatus().code(), a.getOpenDate(),
                a.getExpirationDate(), a.getReissueDate(), a.getCreditLimit(), a.getCashCreditLimit(),
                a.getCurrBal(), a.getCurrCycCredit(), a.getCurrCycDebit(), nz(a.getGroupId()),
                String.format("%09d", c.getCustId()),
                ssn.substring(0, 3) + "-" + ssn.substring(3, 5) + "-" + ssn.substring(5, 9),
                String.format("%03d", c.getFicoCreditScore()), c.getDob(), nz(c.getFirstName()),
                nz(c.getMiddleName()), nz(c.getLastName()), nz(c.getAddrLine1()), nz(c.getAddrLine2()),
                nz(c.getAddrLine3()), nz(c.getAddrStateCd()), nz(c.getAddrZip()), nz(c.getAddrCountryCd()),
                nz(c.getPhoneNum1()), nz(c.getPhoneNum2()), nz(c.getGovtIssuedId()), nz(c.getEftAccountId()),
                c.getPriCardHolderInd() == null ? "" : c.getPriCardHolderInd().code(), details.cardNum(),
                details.cardNumbers(), a.getVersion(), c.getVersion(),
                AccountUpdateRequest.prefilled(AccountChanges.fetched(a, c), a.getVersion(), c.getVersion()), exit);
    }

    private static String nz(String value) {
        return value == null ? "" : value;
    }
}
