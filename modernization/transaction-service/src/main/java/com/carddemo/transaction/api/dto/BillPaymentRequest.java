package com.carddemo.transaction.api.dto;

import jakarta.validation.constraints.NotNull;

/**
 * COBIL00C pays the full current balance of the account, so the request carries no amount:
 * the balance is read from account-service, exactly as the COBOL read ACCTDAT.
 */
public record BillPaymentRequest(@NotNull Long accountId) {
}
