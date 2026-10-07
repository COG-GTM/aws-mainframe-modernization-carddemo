package com.carddemo.web.card;

import com.carddemo.card.online.CardUpdateService;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** Map CCRDUPA after a PUT: the resulting {@code CCUP-CHANGE-ACTION}, its messages and the card as stored. */
@Schema(description = "COCRDUPC result")
public record CardUpdateResponse(
        ScreenHeader header,
        @Schema(description = "SHOW = no change detected (S); VALIDATED = edits passed, not saved (N, send again with "
                + "confirm=true); COMMITTED = card rewritten (C)", example = "COMMITTED")
        CardUpdateService.State state,
        @Schema(description = "true when the card was rewritten", example = "true") boolean updated,
        @Schema(description = "INFOMSG for the state", example = "Changes committed to database") String infoMessage,
        @Schema(description = "ERRMSG: the no-change message, else empty", example = "") String message,
        @Schema(description = "The card as stored after the request (new version after a commit)")
        CardDetailScreen card) {
}
