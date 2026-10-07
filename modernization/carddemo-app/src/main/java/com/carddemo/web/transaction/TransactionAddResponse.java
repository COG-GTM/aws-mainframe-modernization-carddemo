package com.carddemo.web.transaction;

import com.carddemo.transaction.online.TransactionAddService;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** COTRN02C result: validated (awaiting CONFIRM=Y) or added. */
@Schema(description = "Transaction add result")
public record TransactionAddResponse(
        ScreenHeader header,
        @Schema(description = "VALIDATED: edits passed, nothing written; ADDED: written") TransactionAddService.State state,
        @Schema(description = "The form as edited (keys resolved, amount re-edited); send it back with confirm=Y")
        TransactionAddRequest form,
        @Schema(description = "The record written (ADDED only)", nullable = true) TransactionFields transaction,
        @Schema(description = "ERRMSG", example = "Transaction added successfully.  Your Tran ID is 0000000000000301.")
        String message,
        @Schema(description = "PF3: CDEMO-FROM-PROGRAM when given, else COMEN01C") NavigationContext exit) {
}
