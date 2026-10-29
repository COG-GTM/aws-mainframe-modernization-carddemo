package com.carddemo.authfrds.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;

/** COPAUS2C DFHCOMMAREA: WS-ACCT-ID 9(11), WS-CUST-ID 9(9), WS-FRAUD-AUTH-RECORD, WS-FRD-ACTION. */
public record FraudRequest(
        @Schema(example = "1") @NotNull @Min(0) @Max(99999999999L) Long acctId,
        @Schema(example = "1") @NotNull @Min(0) @Max(999999999L) Long custId,
        @Schema(description = "WS-FRD-ACTION: F = report fraud, R = remove fraud", example = "F")
        @NotNull @Pattern(regexp = "[FR]", message = "must be F (report fraud) or R (remove fraud)") String action,
        @NotNull @Valid AuthorizationDetail authorization) {
}
