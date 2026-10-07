package com.carddemo.user.menu;

import com.carddemo.common.online.MessageColor;

/** Outcome of {@link MenuService#select}. {@code option} is the normalised value echoed in {@code OPTIONO}. */
public sealed interface MenuSelection {

    String option();

    /**
     * {@code XCTL PROGRAM(target)} with {@code CDEMO-FROM-TRANID}/{@code CDEMO-FROM-PROGRAM} = the menu and
     * {@code CDEMO-PGM-CONTEXT = 0}.
     */
    record Transfer(String option, MenuOption target, String targetTranId, String fromProgram, String fromTranId)
            implements MenuSelection {
    }

    /** No error flag: the menu is re-sent with an informational message (DUMMY / not-installed rows). */
    record Info(String option, String message, MessageColor color) implements MenuSelection {
    }

    /** Error flag set: the menu is re-sent with the error message. */
    record Rejected(String option, MenuRejection reason, String message) implements MenuSelection {
    }
}
