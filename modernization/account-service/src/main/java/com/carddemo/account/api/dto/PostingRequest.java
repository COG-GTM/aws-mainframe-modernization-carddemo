package com.carddemo.account.api.dto;

import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * One posting against an account, the account side of paragraphs 1500-B-LOOKUP-ACCT and
 * 2800-UPDATE-ACCOUNT-REC of CBTRN02C. {@code transactionDate} is the date part of
 * {@code DALYTRAN-ORIG-TS} and is compared against the account expiration date.
 */
public record PostingRequest(@NotNull BigDecimal amount, @NotNull LocalDate transactionDate) {
}
