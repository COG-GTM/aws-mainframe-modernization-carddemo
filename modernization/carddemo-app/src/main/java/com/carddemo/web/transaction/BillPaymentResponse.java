package com.carddemo.web.transaction;

import com.carddemo.transaction.online.BillPaymentService;
import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** COBIL00C result. */
@Schema(description = "Bill payment result")
public record BillPaymentResponse(
        ScreenHeader header,
        @Schema(description = "SHOW: balance shown, confirm with Y; PAID: paid; CLEARED: N, nothing done")
        BillPaymentService.State state,
        @Schema(description = "ACTIDIN (11 digits)", example = "00000000001", nullable = true) String accountId,
        @Schema(description = "CURBAL: ACCT-CURR-BAL (after the payment: 0.00)", example = "194.00",
                nullable = true) String currentBalance,
        @Schema(description = "Account version to send back with confirm=Y", example = "0", nullable = true)
        Long version,
        @Schema(description = "The bill-payment transaction written (PAID only)", nullable = true)
        TransactionFields transaction,
        @Schema(description = "ERRMSG", example = "Payment successful.  Your Transaction ID is 0000000000000301.")
        String message,
        @Schema(description = "PF3: CDEMO-FROM-PROGRAM when given, else COMEN01C") NavigationContext exit) {
}
