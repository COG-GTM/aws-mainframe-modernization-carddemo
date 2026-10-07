package com.carddemo.web.signon;

import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;

/** First entry to COSGN00C (R-1): an empty sign-on map with the cursor on {@code USERID}. */
@Schema(description = "Empty sign-on screen (COSGN00C R-1)")
public record SignOnScreen(ScreenHeader header, @Schema(example = "") String message,
        @Schema(example = "userId") String cursor) {
}
