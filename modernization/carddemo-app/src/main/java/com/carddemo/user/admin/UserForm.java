package com.carddemo.user.admin;

/**
 * The input fields of COUSR1A/COUSR2A as typed: {@code USERID} (8), {@code FNAME}/{@code LNAME} (20),
 * {@code PASSWD} (8), {@code USRTYPE} (1). Lengths are enforced by the web layer.
 */
public record UserForm(String userId, String firstName, String lastName, String password, String userType) {
}
