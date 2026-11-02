package com.carddemo.auth.service;

import com.carddemo.auth.api.dto.UserRequest;
import com.carddemo.common.error.FieldValidationException;

/**
 * The COUSR01C / COUSR02C screen edits, in the order the COBOL {@code EVALUATE TRUE} checks the map
 * fields; the first failure wins and its text is the COBOL message.
 */
public final class UserEdits {

    public static final String FIRST_NAME_EMPTY = "First Name can NOT be empty...";
    public static final String LAST_NAME_EMPTY = "Last Name can NOT be empty...";
    public static final String USER_ID_EMPTY = "User ID can NOT be empty...";
    public static final String PASSWORD_EMPTY = "Password can NOT be empty...";
    public static final String USER_TYPE_EMPTY = "User Type can NOT be empty...";
    /** Java-only edits (see docs/migration/architecture.md, user administration). */
    public static final String USER_TYPE_INVALID = "User Type must be A (admin) or U (user)...";
    public static final String PASSWORD_TOO_SHORT = "Password must be at least 8 characters...";
    public static final String USER_ID_MISMATCH = "User ID can NOT be changed...";

    private static final int MIN_PASSWORD_LENGTH = 8;

    private UserEdits() {
    }

    /** COUSR01C PROCESS-ENTER-KEY: first name, last name, user id, password, user type. */
    public static void forAdd(UserRequest request) {
        require("firstName", request.firstName(), FIRST_NAME_EMPTY);
        require("lastName", request.lastName(), LAST_NAME_EMPTY);
        require("userId", request.userId(), USER_ID_EMPTY);
        require("password", request.password(), PASSWORD_EMPTY);
        require("userType", request.userType(), USER_TYPE_EMPTY);
        javaRules(request);
    }

    /** COUSR02C UPDATE-USER-INFO: user id, first name, last name, password, user type. */
    public static void forUpdate(String pathUserId, UserRequest request) {
        if (!blank(request.userId()) && !request.userId().trim().equalsIgnoreCase(pathUserId.trim())) {
            throw new FieldValidationException("userId", USER_ID_MISMATCH);
        }
        require("firstName", request.firstName(), FIRST_NAME_EMPTY);
        require("lastName", request.lastName(), LAST_NAME_EMPTY);
        require("password", request.password(), PASSWORD_EMPTY);
        require("userType", request.userType(), USER_TYPE_EMPTY);
        javaRules(request);
    }

    /**
     * Stricter than the COBOL, which stores any non-blank type and an 8-byte clear-text password:
     * COSGN00C/COADM01C only recognise {@code A} as admin, and the password is BCrypt-hashed.
     */
    private static void javaRules(UserRequest request) {
        if (!"A".equals(request.userType()) && !"U".equals(request.userType())) {
            throw new FieldValidationException("userType", USER_TYPE_INVALID);
        }
        if (request.password().length() < MIN_PASSWORD_LENGTH) {
            throw new FieldValidationException("password", PASSWORD_TOO_SHORT);
        }
    }

    static void require(String field, String value, String message) {
        if (blank(value)) {
            throw new FieldValidationException(field, message);
        }
    }

    /** COBOL {@code = SPACES OR LOW-VALUES}. */
    static boolean blank(String value) {
        return value == null || value.chars().allMatch(c -> c == ' ' || c == 0);
    }
}
