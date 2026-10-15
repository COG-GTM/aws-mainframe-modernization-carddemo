package com.carddemo.common.security;

/** Replaces CDEMO-USER-ID / CDEMO-USER-TYPE carried in the CICS COMMAREA (COCOM01Y). */
public record CardDemoPrincipal(String userId, String userType, String firstName, String lastName) {

    public static final String ADMIN = "A";
    public static final String USER = "U";

    public boolean isAdmin() {
        return ADMIN.equals(userType);
    }

    public String role() {
        return isAdmin() ? "ROLE_ADMIN" : "ROLE_USER";
    }
}
