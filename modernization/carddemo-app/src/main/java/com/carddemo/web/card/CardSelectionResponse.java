package com.carddemo.web.card;

import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** ENTER on the list: the {@code XCTL} to COCRDSLC/COCRDUPC for the selected row, or stay on the list. */
@Schema(description = "COCRDLIC selection result")
public record CardSelectionResponse(
        ScreenHeader header,
        @Schema(description = "XCTL target with the selected account (cardNum is left out: the PAN travels as "
                + "cardRef); null when nothing was selected", nullable = true) NavigationContext navigation,
        @Schema(description = "cardRef of the selected card", nullable = true) String cardRef,
        @Schema(description = "Request to send next: the detail (S) or the card to update (U)", nullable = true,
                example = "/api/v1/cards/kV3t2b6mV0sWZ0lq8nO4bQ?accountId=00000000050&fromProgram=COCRDLIC")
        String next,
        @Schema(description = "INFOMSG (no selection: stay on the list)", example = "") String infoMessage) {
}
