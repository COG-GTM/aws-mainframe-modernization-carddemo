package com.carddemo.account.client;

/** card-service {@code GET /api/v1/card-xrefs/by-account/{accountId}}: the CXACAIX record. */
public record CardXrefView(String cardNumber, Long customerId, Long accountId) {
}
