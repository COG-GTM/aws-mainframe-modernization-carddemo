package com.carddemo.card;

import java.time.LocalDate;

public record CardRecord(String cardNum, long acctId, int cvvCd, String embossedName, LocalDate expirationDate,
        String activeStatus, long version) {
}
