package com.carddemo.transactiontype.api;

import io.swagger.v3.oas.annotations.media.Schema;

public record TransactionTypeRequest(
        @Schema(example = "08", description = "TR_TYPE: 1-2 digits, non-zero; '8' is stored as '08'") String code,
        @Schema(example = "CASHBACK", description = "TR_DESCRIPTION: letters, digits and spaces, max 50") String description) {
}
