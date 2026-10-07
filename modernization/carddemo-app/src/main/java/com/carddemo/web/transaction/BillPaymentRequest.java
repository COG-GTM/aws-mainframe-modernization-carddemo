package com.carddemo.web.transaction;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/** COBIL0A {@code CONFIRM}, plus the account version that was shown with the balance. */
@Schema(description = "Bill payment (COBIL00C / map COBIL0A)")
public record BillPaymentRequest(
        @Schema(description = "CONFIRM: blank = show the balance, Y = pay it in full, N = clear", example = "Y")
        @Size(max = 1) String confirm,
        @Schema(description = "Account version returned with the balance shown; required with Y (409 CHANGED when "
                + "the account changed or was paid since)", example = "0", nullable = true) Long version) {
}
