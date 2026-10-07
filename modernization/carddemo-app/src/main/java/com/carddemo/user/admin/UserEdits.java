package com.carddemo.user.admin;

import com.carddemo.common.FieldEditException;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.user.UserType;
import java.util.List;

/** Field edits shared by COUSR01C {@code PROCESS-ENTER-KEY} and COUSR02C {@code UPDATE-USER-INFO}. */
final class UserEdits {

    private UserEdits() {
    }

    /** A blank field fails with its message; the first failing field wins (COUSR01C R-8..R-12, COUSR02C R-11..R-15). */
    static void required(String value, String field, String message) {
        if (ScreenInput.isSpacesOrLowValues(value)) {
            throw new FieldEditException(field, message, List.of(field));
        }
    }

    /**
     * {@code USRTYPE}: the COBOL stores any character, but only {@code A}/{@code U} mean anything to COSGN00C and the
     * {@code UserType} level-88s (ADR-0006), so other values are rejected; {@code a}/{@code u} are upper-cased.
     */
    static UserType userType(String typed) {
        String code = typed.strip().toUpperCase(java.util.Locale.ROOT);
        for (UserType type : UserType.values()) {
            if (type.code().equals(code)) {
                return type;
            }
        }
        throw new FieldEditException(UserAdminMessages.USER_TYPE_FIELD, UserAdminMessages.MSG_USER_TYPE_INVALID,
                List.of(UserAdminMessages.USER_TYPE_FIELD));
    }

    /** PIC X values are stored without trailing spaces (ADR-0003). */
    static String text(String typed) {
        return ScreenInput.rightTrim(typed);
    }
}
