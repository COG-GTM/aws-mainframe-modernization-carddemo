package com.carddemo.web.card;

import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

/** Map CCRDLIA (COCRDLIC) after one browse request. */
@Schema(description = "COCRDLIC card list screen: seven rows per page in card-number order")
public record CardListScreen(
        ScreenHeader header,
        @Schema(description = "ACCTSID filter as edited (11 digits), null when blank", example = "00000000050",
                nullable = true) String accountId,
        @Schema(description = "CARDSID filter as edited, masked; null when blank", example = "************5740",
                nullable = true) String cardNumber,
        @Schema(description = "WS-MAX-SCREEN-LINES", example = "7") int pageSize,
        List<CardListRow> rows,
        @Schema(description = "PF7 would show an earlier page") boolean hasPreviousPage,
        @Schema(description = "PF8 would show a later page (CA-NEXT-PAGE-EXISTS)") boolean hasNextPage,
        @Schema(description = "PF7: pass as before= (the first row's cardRef); null when there is no previous page",
                nullable = true) String previousPage,
        @Schema(description = "PF8: pass as after= (the last row's cardRef); null when there is no next page",
                nullable = true) String nextPage,
        @Schema(description = "INFOMSG", example = "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD") String infoMessage,
        @Schema(description = "ERRMSG", example = "") String message,
        @Schema(description = "PF3: COMEN01C") NavigationContext exit) {
}
