package com.carddemo.account.api.dto;

import com.carddemo.account.domain.PostingType;
import jakarta.validation.constraints.Digits;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * One posting against an account, the account side of paragraphs 1500-B-LOOKUP-ACCT and
 * 2800-UPDATE-ACCOUNT-REC of CBTRN02C. {@code transactionDate} is the date part of
 * {@code DALYTRAN-ORIG-TS} and is compared against the account expiration date. {@code transactionId}
 * ({@code DALYTRAN-ID}, {@code TRAN-ID}) is the idempotency key: a posting is applied at most once per id.
 * {@code type} {@code BILL_PAYMENT} applies COBIL00C instead (balance only, no 102/103 checks).
 */
public record PostingRequest(@NotBlank @Size(max = 16) String transactionId,
                             @NotNull @Digits(integer = 10, fraction = 2) BigDecimal amount, @NotNull LocalDate transactionDate,
                             PostingType type) {

    /** Absent means a CBTRN02C daily transaction posting. */
    public PostingType typeOrDefault() {
        return type == null ? PostingType.DAILY_TRANSACTION : type;
    }
}
