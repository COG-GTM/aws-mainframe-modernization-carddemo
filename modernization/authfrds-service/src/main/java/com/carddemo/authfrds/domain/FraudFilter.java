package com.carddemo.authfrds.domain;

public record FraudFilter(String cardNum, Long acctId, String authFraud) {
}
