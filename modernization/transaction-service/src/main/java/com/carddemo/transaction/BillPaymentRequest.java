package com.carddemo.transaction;

public record BillPaymentRequest(String acctId, String confirm) {
}
