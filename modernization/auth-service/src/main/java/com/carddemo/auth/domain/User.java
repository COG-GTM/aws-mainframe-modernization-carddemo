package com.carddemo.auth.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 * USRSEC record, copybook CSUSR01Y. SEC-USR-PWD held an eight byte clear text password; the
 * modern column holds a BCrypt hash instead and is never returned by the API.
 */
@Entity
@Table(name = "users")
public class User {

    @Id
    @Column(name = "user_id", length = 8, nullable = false)
    private String userId;

    @Column(name = "first_name", length = 20)
    private String firstName;

    @Column(name = "last_name", length = 20)
    private String lastName;

    @Column(name = "password_hash", length = 100, nullable = false)
    private String passwordHash;

    /** SEC-USR-TYPE: 'A' admin (COADM01C menu), 'U' regular user (COMEN01C menu). */
    @Column(name = "user_type", length = 1, nullable = false)
    private String userType;

    protected User() {
    }

    public User(String userId, String passwordHash, String userType) {
        this.userId = userId;
        this.passwordHash = passwordHash;
        this.userType = userType;
    }

    public String getUserId() {
        return userId;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getPasswordHash() {
        return passwordHash;
    }

    public void setPasswordHash(String passwordHash) {
        this.passwordHash = passwordHash;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    public boolean isAdmin() {
        return "A".equalsIgnoreCase(userType);
    }
}
