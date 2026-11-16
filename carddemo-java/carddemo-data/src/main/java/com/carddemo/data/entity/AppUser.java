package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/** USRSEC (copybook CSUSR01Y), VSAM KSDS keyed by SEC-USR-ID. The COBOL clear-text SEC-USR-PWD is kept only as a hash. */
@Entity
@Table(name = "app_user")
public class AppUser {

    @Id
    @Column(name = "user_id", nullable = false, length = 8)
    private String userId;

    @Column(name = "first_name", nullable = false, length = 20)
    private String firstName;

    @Column(name = "last_name", nullable = false, length = 20)
    private String lastName;

    @Column(name = "password_hash", nullable = false, length = 100)
    private String passwordHash;

    @Column(name = "user_type", nullable = false, length = 1)
    private String userType;

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
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

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof AppUser other && userId != null && userId.equals(other.getUserId()));
    }

    @Override
    public int hashCode() {
        return AppUser.class.hashCode();
    }
}
