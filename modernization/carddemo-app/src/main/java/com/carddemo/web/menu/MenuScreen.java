package com.carddemo.web.menu;

import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

/**
 * {@code SEND-MENU-SCREEN} of COMEN01C (R-13/R-14) / COADM01C (R-12/R-13).
 *
 * @param optionLines exactly the {@code OPTN001O..OPTN012O} fields: labels, then blanks
 */
@Schema(description = "Menu screen (COMEN1A / COADM1A)")
public record MenuScreen(
        ScreenHeader header,
        @Schema(example = "main", allowableValues = {"main", "admin"}) String menu,
        @Schema(example = "COMEN01C") String programId,
        @Schema(example = "CM00") String tranId,
        @Schema(example = "COMEN01") String mapset,
        @Schema(example = "COMEN1A") String map,
        List<MenuOptionView> options,
        List<String> optionLines,
        @Schema(example = "") String message) {
}
