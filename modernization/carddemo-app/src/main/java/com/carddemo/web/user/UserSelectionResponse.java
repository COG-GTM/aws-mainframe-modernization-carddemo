package com.carddemo.web.user;

import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** ENTER on the list: the {@code XCTL} to COUSR02C (U) or COUSR03C (D) for the selected row, or stay. */
@Schema(description = "COUSR00C selection result")
public record UserSelectionResponse(
        ScreenHeader header,
        @Schema(description = "XCTL target (COUSR02C or COUSR03C); null when nothing was selected", nullable = true)
        NavigationContext navigation,
        @Schema(description = "CDEMO-CU00-USR-SELECTED", nullable = true, example = "USER0001") String userId,
        @Schema(description = "Request to send next", nullable = true,
                example = "GET /api/v1/users/USER0001?fromProgram=COUSR00C") String next) {
}
