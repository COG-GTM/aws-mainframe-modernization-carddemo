package com.carddemo.user;

import com.fasterxml.jackson.annotation.JsonInclude;

public final class UserDtos {

    private UserDtos() {
    }

    public record UserSummary(String userId, String firstName, String lastName, String userType) {
    }

    public record UserDetail(String userId, String firstName, String lastName, String userType, long version,
            @JsonInclude(JsonInclude.Include.NON_NULL) String message) {
    }

    public record CreateUserRequest(String userId, String firstName, String lastName, String password,
            String userType) {
    }

    public record UpdateUserRequest(String firstName, String lastName, String password, String userType,
            Long version) {
    }
}
