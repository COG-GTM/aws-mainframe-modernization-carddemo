package com.carddemo.user.admin;

/** Message texts of COUSR00C-COUSR03C, as the programs write them to {@code ERRMSGO}. */
public final class UserAdminMessages {

    public static final String USER_ID_FIELD = "userId";
    public static final String FIRST_NAME_FIELD = "firstName";
    public static final String LAST_NAME_FIELD = "lastName";
    public static final String PASSWORD_FIELD = "password";
    public static final String USER_TYPE_FIELD = "userType";
    public static final String VERSION_FIELD = "version";
    public static final String CONFIRM_FIELD = "confirm";

    public static final String MSG_USER_ID_EMPTY = "User ID can NOT be empty...";
    public static final String MSG_FIRST_NAME_EMPTY = "First Name can NOT be empty...";
    public static final String MSG_LAST_NAME_EMPTY = "Last Name can NOT be empty...";
    public static final String MSG_PASSWORD_EMPTY = "Password can NOT be empty...";
    public static final String MSG_USER_TYPE_EMPTY = "User Type can NOT be empty...";
    /** Not in the COBOL (any character was stored): USRSEC types are only A and U (ADR-0006), see COUSR01C.md. */
    public static final String MSG_USER_TYPE_INVALID = "User Type must be A (Admin) or U (User)...";
    public static final String MSG_DUPLICATE = "User ID already exist...";
    public static final String MSG_ADD_FAILED = "Unable to Add User...";
    public static final String MSG_NOT_FOUND = "User ID NOT found...";
    public static final String MSG_LOOKUP_FAILED = "Unable to lookup User...";
    public static final String MSG_PRESS_PF5_TO_UPDATE = "Press PF5 key to save your updates ...";
    public static final String MSG_NO_CHANGES = "Please modify to update ...";
    /** COUSR02C REWRITE and COUSR03C DELETE failures share this text (COUSR03C R-19: "sic"). */
    public static final String MSG_UPDATE_FAILED = "Unable to Update User...";
    public static final String MSG_PRESS_PF5_TO_DELETE = "Press PF5 key to delete this user ...";
    public static final String MSG_VERSION_REQUIRED =
            "version is required: send the version of the user shown (READ UPDATE re-check, ADR-0010)";

    private UserAdminMessages() {
    }

    /** {@code User <id> has been added ...}: SEC-USR-ID up to its first space (COUSR01C R-15). */
    public static String added(String userId) {
        return "User " + upToFirstSpace(userId) + " has been added ...";
    }

    public static String updated(String userId) {
        return "User " + upToFirstSpace(userId) + " has been updated ...";
    }

    public static String deleted(String userId) {
        return "User " + upToFirstSpace(userId) + " has been deleted ...";
    }

    /** {@code "<value>" is not a valid value to confirm...} with the value delimited by space (CORPT00C R-17). */
    public static String invalidConfirm(String confirm) {
        return "\"" + upToFirstSpace(confirm.strip()) + "\" is not a valid value to confirm...";
    }

    static String upToFirstSpace(String text) {
        int space = text.indexOf(' ');
        return space < 0 ? text : text.substring(0, space);
    }
}
