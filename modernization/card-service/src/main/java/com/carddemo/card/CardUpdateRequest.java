package com.carddemo.card;

public record CardUpdateRequest(String embossedName, String activeStatus, Integer expiryMonth, Integer expiryYear,
                                long version) {
}
