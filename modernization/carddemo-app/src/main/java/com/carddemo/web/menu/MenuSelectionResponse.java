package com.carddemo.web.menu;

import com.carddemo.common.online.MessageColor;
import com.carddemo.web.NavigationContext;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * A selection that did not set the error flag: either an {@code XCTL} ({@code navigation} set, {@code message}
 * empty) or the menu re-sent with an informational message ({@code navigation} null).
 *
 * @param option normalised option echoed in {@code OPTIONO} (R-7)
 */
@Schema(description = "Menu selection result: navigation to the target program, or an informational message")
public record MenuSelectionResponse(
        @Schema(example = "01") String option,
        @Schema(nullable = true) NavigationContext navigation,
        @Schema(example = "") String message,
        @Schema(example = "DEFAULT") MessageColor messageColor) {
}
