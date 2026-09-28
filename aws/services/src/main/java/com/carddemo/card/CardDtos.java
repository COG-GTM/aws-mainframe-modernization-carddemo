package com.carddemo.card;

import com.fasterxml.jackson.annotation.JsonInclude;

public final class CardDtos {

    private CardDtos() {
    }

    public record CardSummary(String cardNum, long acctId, String activeStatus) {
    }

    public record CardDetail(String cardNum, long acctId, int cvvCd, String embossedName, String expirationDate,
            String activeStatus, long version, @JsonInclude(JsonInclude.Include.NON_NULL) String message) {
    }

    public record CardUpdateRequest(String acctId, String embossedName, String activeStatus, String expirationDate,
            Long version) {
    }
}
