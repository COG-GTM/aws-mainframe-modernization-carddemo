package com.carddemo.user;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;

/**
 * JPA entity for table {@code user_security}. Signon security record (USRSEC KSDS, key SEC-USR-ID); read by
 * COSGN00C, maintained by COUSR01C-03C. Field Javadocs carry the CSUSR01Y item names; {@link UserSecurityRecord} is
 * the fixed-width value.
 */
@Entity
@Table(name = "user_security")
public class UserSecurity {

    /** SEC-USR-ID PIC X(08). */
    @Id
    @Column(name = "usr_id")
    private String usrId;

    /** SEC-USR-FNAME PIC X(20). */
    @Column(name = "first_name")
    private String firstName;

    /** SEC-USR-LNAME PIC X(20). */
    @Column(name = "last_name")
    private String lastName;

    /** SEC-USR-PWD PIC X(08). */
    @Column(name = "password")
    private String password;

    /** SEC-USR-TYPE PIC X(01). */
    @Column(name = "usr_type")
    private UserType usrType;

    /** Optimistic-lock version (ADR-0010); not part of the VSAM record. */
    @Version
    @Column(name = "version")
    private long version;

    protected UserSecurity() {
    }

    public static UserSecurity from(UserSecurityRecord record) {
        UserSecurity entity = new UserSecurity();
        entity.usrId = record.usrId();
        entity.firstName = record.firstName();
        entity.lastName = record.lastName();
        entity.password = record.password();
        entity.usrType = record.usrType();
        return entity;
    }

    public UserSecurityRecord toRecord() {
        return new UserSecurityRecord(usrId, firstName, lastName, password, usrType);
    }

    /** {@code REWRITE}: replaces every non-key field with the values of {@code record}; the key must match. */
    public void update(UserSecurityRecord record) {
        if (!usrId.equals(record.usrId())) {
            throw new IllegalArgumentException("user_security: cannot change the key of " + usrId + " via update");
        }
        this.firstName = record.firstName();
        this.lastName = record.lastName();
        this.password = record.password();
        this.usrType = record.usrType();
    }

    public String getUsrId() {
        return usrId;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getPassword() {
        return password;
    }

    public UserType getUsrType() {
        return usrType;
    }

    public long getVersion() {
        return version;
    }
}
