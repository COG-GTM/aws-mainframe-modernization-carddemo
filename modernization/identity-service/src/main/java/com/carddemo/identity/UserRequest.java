package com.carddemo.identity;

public record UserRequest(String userId, String firstName, String lastName, String password, String userType) {
}
