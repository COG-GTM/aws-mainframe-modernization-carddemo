package com.carddemo.user;

public record UserRecord(String userId, String firstName, String lastName, String passwordHash, String userType,
        long version) {
}
