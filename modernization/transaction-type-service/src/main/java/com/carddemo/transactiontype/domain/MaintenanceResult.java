package com.carddemo.transactiontype.domain;

public record MaintenanceResult(TransactionType item, String message, boolean created) {
}
