package com.carddemo.identity;

import com.carddemo.common.domain.UserSecurity;

/** Never exposes SEC-USR-PWD (the 3270 screens displayed it in a dark field). */
public record UserView(String userId, String firstName, String lastName, String userType) {

    static UserView of(UserSecurity u) {
        return new UserView(u.getUserId(), u.getFirstName(), u.getLastName(), u.getUserType());
    }
}
