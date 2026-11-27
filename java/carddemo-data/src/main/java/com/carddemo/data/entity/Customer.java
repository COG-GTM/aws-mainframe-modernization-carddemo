package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.time.LocalDate;

/**
 * Customer (VSAM KSDS CUSTDAT, RECLN 500).
 * <p>Source copybook: CVCUS01Y.
 */
@Entity
@Table(name = "customer")
public class Customer {

    /** CUST-ID PIC 9(09) - VSAM KSDS key (CUSTDAT) */
    @Id
    @Column(name = "cust_id")
    private Long id;

    /** CUST-FIRST-NAME PIC X(25) */
    @Column(name = "first_name", length = 25)
    private String firstName;

    /** CUST-MIDDLE-NAME PIC X(25) */
    @Column(name = "middle_name", length = 25)
    private String middleName;

    /** CUST-LAST-NAME PIC X(25) */
    @Column(name = "last_name", length = 25)
    private String lastName;

    /** CUST-PHONE-NUM-1 PIC X(15) */
    @Column(name = "phone_num_1", length = 15)
    private String phoneNumber1;

    /** CUST-PHONE-NUM-2 PIC X(15) */
    @Column(name = "phone_num_2", length = 15)
    private String phoneNumber2;

    /** CUST-SSN PIC 9(09), kept as text to preserve leading zeros */
    @Column(name = "ssn", length = 9)
    private String ssn;

    /** CUST-GOVT-ISSUED-ID PIC X(20) */
    @Column(name = "govt_issued_id", length = 20)
    private String governmentIssuedId;

    /** CUST-DOB-YYYY-MM-DD PIC X(10) */
    @Column(name = "dob")
    private LocalDate dateOfBirth;

    /** CUST-EFT-ACCOUNT-ID PIC X(10) */
    @Column(name = "eft_account_id", length = 10)
    private String eftAccountId;

    /** CUST-PRI-CARD-HOLDER-IND PIC X(01) */
    @Column(name = "pri_card_holder_ind", length = 1)
    private String primaryCardHolderIndicator;

    /** CUST-FICO-CREDIT-SCORE PIC 9(03) */
    @Column(name = "fico_credit_score")
    private Integer ficoCreditScore;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getMiddleName() {
        return middleName;
    }

    public void setMiddleName(String middleName) {
        this.middleName = middleName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getPhoneNumber1() {
        return phoneNumber1;
    }

    public void setPhoneNumber1(String phoneNumber1) {
        this.phoneNumber1 = phoneNumber1;
    }

    public String getPhoneNumber2() {
        return phoneNumber2;
    }

    public void setPhoneNumber2(String phoneNumber2) {
        this.phoneNumber2 = phoneNumber2;
    }

    public String getSsn() {
        return ssn;
    }

    public void setSsn(String ssn) {
        this.ssn = ssn;
    }

    public String getGovernmentIssuedId() {
        return governmentIssuedId;
    }

    public void setGovernmentIssuedId(String governmentIssuedId) {
        this.governmentIssuedId = governmentIssuedId;
    }

    public LocalDate getDateOfBirth() {
        return dateOfBirth;
    }

    public void setDateOfBirth(LocalDate dateOfBirth) {
        this.dateOfBirth = dateOfBirth;
    }

    public String getEftAccountId() {
        return eftAccountId;
    }

    public void setEftAccountId(String eftAccountId) {
        this.eftAccountId = eftAccountId;
    }

    public String getPrimaryCardHolderIndicator() {
        return primaryCardHolderIndicator;
    }

    public void setPrimaryCardHolderIndicator(String primaryCardHolderIndicator) {
        this.primaryCardHolderIndicator = primaryCardHolderIndicator;
    }

    public Integer getFicoCreditScore() {
        return ficoCreditScore;
    }

    public void setFicoCreditScore(Integer ficoCreditScore) {
        this.ficoCreditScore = ficoCreditScore;
    }

    /** CUST-ADDR-LINE-1 .. CUST-ADDR-ZIP */
    @Embedded
    private Address address = new Address();

    public Address getAddress() {
        return address;
    }

    public void setAddress(Address address) {
        this.address = address;
    }
}
