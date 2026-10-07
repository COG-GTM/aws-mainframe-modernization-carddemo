package com.carddemo.web.menu;

import com.carddemo.user.menu.MenuOption;
import io.swagger.v3.oas.annotations.media.Schema;

/** One option of a menu as the BMS map shows it ({@code OPTN0nnO}) plus its target. */
@Schema(description = "Menu option (COMEN02Y / COADM02Y row)")
public record MenuOptionView(
        @Schema(example = "1") int number,
        @Schema(description = "OPTN0nnO text: '<NN>. <name>'", example = "01. Account View") String label,
        @Schema(example = "Account View") String name,
        @Schema(description = "XCTL target", example = "COACTVWC") String programId,
        @Schema(description = "CDEMO-MENU-OPT-USRTYPE = 'A' (main menu only)") boolean adminOnly) {

    static MenuOptionView of(MenuOption option) {
        return new MenuOptionView(option.number(), option.label(), option.name(), option.programId(),
                option.adminOnly());
    }
}
