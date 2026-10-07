package com.carddemo.web.transaction;

import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** ENTER on the list: the {@code XCTL} to COTRN01C for the selected row, or stay on the list. */
@Schema(description = "COTRN00C selection result")
public record TransactionSelectionResponse(
        ScreenHeader header,
        @Schema(description = "XCTL target (COTRN01C); null when nothing was selected", nullable = true)
        NavigationContext navigation,
        @Schema(description = "CDEMO-CT00-TRN-SELECTED", nullable = true, example = "0000000000000001")
        String tranId,
        @Schema(description = "Request to send next", nullable = true,
                example = "/api/v1/transactions/0000000000000001?fromProgram=COTRN00C") String next) {
}
