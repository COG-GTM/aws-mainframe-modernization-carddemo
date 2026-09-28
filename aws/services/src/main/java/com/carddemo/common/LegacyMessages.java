package com.carddemo.common;

/** Business messages shared by several legacy programs (see CSMSG01Y / CSMSG02Y and the program sources). */
public final class LegacyMessages {

    public static final String RECORD_CHANGED = "Record changed by some one else. Please review";
    public static final String NO_CHANGE = "No change detected with respect to values fetched.";
    public static final String CHANGES_COMMITTED = "Changes committed to database";
    public static final String COULD_NOT_LOCK = "Could not lock record for update";
    public static final String UPDATE_FAILED = "Update of record failed";
    public static final String ADMIN_ONLY = "No access - Admin Only option... ";
    public static final String INVALID_KEY = "Invalid key pressed. Please see below...         ";

    private LegacyMessages() {
    }
}
