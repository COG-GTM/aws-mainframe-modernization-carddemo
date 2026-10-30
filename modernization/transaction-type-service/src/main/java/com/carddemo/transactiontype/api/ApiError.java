package com.carddemo.transactiontype.api;

public record ApiError(int status, String error, String ruleId, String message, Integer sqlCode) {
}
