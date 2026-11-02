package com.carddemo.transaction.api.dto;

import jakarta.validation.constraints.NotNull;

/**
 * COBIL00C pays the full current balance of the account, so the request carries no amount:
 * the balance is read from account-service, exactly as the COBOL read ACCTDAT. The request itself is
 * the {@code Y} confirmation; COBIL00C's blank confirmation (show the balance) is
 * {@code GET /api/v1/accounts/{accountId}}.
 */
public record BillPaymentRequest(@NotNull(message = "Acct ID can NOT be empty...") Long accountId) {
}
