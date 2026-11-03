package com.carddemo.auth.api.dto;

import com.carddemo.auth.domain.User;

public record UserResponse(String userId, String firstName, String lastName, String userType) {

    public static UserResponse from(User user) {
        return new UserResponse(user.getUserId(), user.getFirstName(), user.getLastName(), user.getUserType());
    }
}
