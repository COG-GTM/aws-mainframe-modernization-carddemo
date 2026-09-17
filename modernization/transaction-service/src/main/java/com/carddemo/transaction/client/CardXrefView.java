package com.carddemo.transaction.client;

/** CARDXREF projection returned by card-service. */
public record CardXrefView(String cardNumber, Long customerId, Long accountId) {
}
