package com.carddemo.user.signon;

/** Why {@code PROCESS-ENTER-KEY} / {@code READ-USER-SEC-FILE} of COSGN00C re-sent the sign-on screen. */
public enum SignOnFailure {
    /** R-5: {@code USERIDI} is {@code SPACES} or {@code LOW-VALUES}. */
    USER_ID_BLANK,
    /** R-6: {@code PASSWDI} is {@code SPACES} or {@code LOW-VALUES}. */
    PASSWORD_BLANK,
    /** R-12: record found, {@code SEC-USR-PWD} differs. */
    WRONG_PASSWORD,
    /** R-13: {@code RESP = NOTFND}. */
    USER_NOT_FOUND,
    /** R-14: any other RESP. */
    UNABLE_TO_VERIFY
}
