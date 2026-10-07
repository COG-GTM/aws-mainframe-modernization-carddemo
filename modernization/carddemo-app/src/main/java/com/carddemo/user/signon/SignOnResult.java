package com.carddemo.user.signon;

import com.carddemo.user.UserType;

/** Outcome of {@link SignOnService#signOn}: the signed-on user (R-9) or the re-sent screen's message. */
public sealed interface SignOnResult {

    /**
     * R-9: credentials match. {@link #targetProgram()} is the {@code XCTL} target of R-10/R-11.
     *
     * @param userId   {@code CDEMO-USER-ID}, upper-cased
     * @param userType {@code CDEMO-USER-TYPE} ({@code SEC-USR-TYPE})
     */
    record SignedOn(String userId, UserType userType, String firstName, String lastName) implements SignOnResult {

        /** R-10 admin menu {@code COADM01C} for type {@code 'A'}, R-11 main menu {@code COMEN01C} otherwise. */
        public String targetProgram() {
            return userType == UserType.ADMIN ? SignOnService.ADMIN_MENU_PROGRAM : SignOnService.MAIN_MENU_PROGRAM;
        }
    }

    /**
     * The screen is re-sent with {@code message} in {@code ERRMSG} and the cursor on {@code field}.
     *
     * @param field request field of the BMS field holding the cursor: {@code userId} or {@code password}
     */
    record Rejected(SignOnFailure reason, String field, String message) implements SignOnResult {
    }
}
