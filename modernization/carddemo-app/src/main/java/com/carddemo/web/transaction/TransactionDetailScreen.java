package com.carddemo.web.transaction;

import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** COTRN1A: one transaction. */
@Schema(description = "Transaction detail screen (COTRN01C / map COTRN1A)")
public record TransactionDetailScreen(
        ScreenHeader header,
        TransactionFields transaction,
        @Schema(description = "ERRMSG", example = "") String message,
        @Schema(description = "PF3: CDEMO-FROM-PROGRAM when given, else COMEN01C (R-5)") NavigationContext exit,
        @Schema(description = "PF5: back to the transaction list COTRN00C (R-7)") NavigationContext list) {
}
