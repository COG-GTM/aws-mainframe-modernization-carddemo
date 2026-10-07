package com.carddemo.user.menu;

/** Selections that set {@code WS-ERR-FLG} and re-send the menu. */
public enum MenuRejection {
    /** COMEN01C R-8 / COADM01C R-8: not numeric, zero or above the option count. */
    INVALID_OPTION,
    /** COMEN01C R-9: a user selected an option whose type is {@code 'A'}. */
    ADMIN_ONLY
}
