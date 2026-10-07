package com.carddemo.web.account;

import com.carddemo.account.online.AccountUpdateService;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** Map {@code CACTUPA} after a PUT: the resulting {@code ACUP-CHANGE-ACTION}, its messages and the account as stored. */
@Schema(description = "COACTUPC result")
public record AccountUpdateResponse(
        ScreenHeader header,
        @Schema(description = "SHOW = no change detected (S); VALIDATED = edits passed, not saved (N, send again with "
                + "confirm=true); COMMITTED = account and customer rewritten (C)", example = "COMMITTED")
        AccountUpdateService.State state,
        @Schema(description = "true when the account and customer were rewritten", example = "true") boolean updated,
        @Schema(description = "INFOMSG for the state", example = "Changes committed to database") String infoMessage,
        @Schema(description = "ERRMSG: the no-change message, else empty", example = "") String message,
        @Schema(description = "The account as stored after the request (new versions after a commit)")
        AccountViewScreen account) {
}
