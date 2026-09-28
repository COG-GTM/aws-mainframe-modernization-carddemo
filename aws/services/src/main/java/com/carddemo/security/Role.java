package com.carddemo.security;

/** Maps SEC-USR-TYPE ('A' admin, 'U' regular user) to API roles. */
public enum Role {
    ADMIN, USER;

    public static Role fromUserType(String userType) {
        return "A".equalsIgnoreCase(userType == null ? "" : userType.strip()) ? ADMIN : USER;
    }
}
