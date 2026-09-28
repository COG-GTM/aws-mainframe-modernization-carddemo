package com.carddemo.account;

import java.time.LocalDate;

public record CustomerRecord(
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
        LocalDate dob,
        String eftAccountId,
        String priCardHolderInd,
        Integer ficoCreditScore,
        long version) {
}
