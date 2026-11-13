package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 * Sign-on user (VSAM KSDS USRSEC, RECLN 80).
 * <p>Source copybook: CSUSR01Y.
 */
@Entity
@Table(name = "user_security")
public class UserSecurity {

    /** SEC-USR-ID PIC X(08) - VSAM KSDS key (USRSEC) */
    @Id
    @Column(name = "user_id", length = 8)
    private String userId;

    /** SEC-USR-FNAME PIC X(20) */
    @Column(name = "first_name", length = 20)
    private String firstName;

    /** SEC-USR-LNAME PIC X(20) */
    @Column(name = "last_name", length = 20)
    private String lastName;

    /** SEC-USR-PWD PIC X(08) - legacy clear text; hashing is an online-track decision */
    @Column(name = "password", length = 8)
    private String password;

    /** SEC-USR-TYPE PIC X(01): A = admin, U = regular user */
    @Convert(converter = UserType.Converter.class)
    @Column(name = "user_type", length = 1)
    private UserType userType;

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

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public UserType getUserType() {
        return userType;
    }

    public void setUserType(UserType userType) {
        this.userType = userType;
    }
}
