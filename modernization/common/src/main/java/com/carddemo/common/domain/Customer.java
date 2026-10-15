package com.carddemo.common.domain;

import jakarta.persistence.*;
import java.time.LocalDate;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table customer; legacy source CUSTDAT / CVCUS01Y. */
@Entity
@Table(name = "customer")
public class Customer {

    @Id
    @Column(name = "cust_id")
    private Long custId;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "middle_name")
    private String middleName;

    @Column(name = "last_name")
    private String lastName;

    @Column(name = "addr_line_1")
    private String addrLine1;

    @Column(name = "addr_line_2")
    private String addrLine2;

    @Column(name = "addr_line_3")
    private String addrLine3;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "addr_state_cd")
    private String addrStateCd;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "addr_country_cd")
    private String addrCountryCd;

    @Column(name = "addr_zip")
    private String addrZip;

    @Column(name = "phone_num_1")
    private String phoneNum1;

    @Column(name = "phone_num_2")
    private String phoneNum2;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "ssn")
    private String ssn;

    @Column(name = "govt_issued_id")
    private String govtIssuedId;

    @Column(name = "dob")
    private LocalDate dob;

    @Column(name = "eft_account_id")
    private String eftAccountId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "pri_card_holder_ind")
    private String priCardHolderInd;

    @Column(name = "fico_credit_score")
    private Integer ficoCreditScore;

    @Version
    @Column(name = "version")
    private long version;

    public Long getCustId() {
        return custId;
    }

    public void setCustId(Long custId) {
        this.custId = custId;
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

    public String getAddrLine1() {
        return addrLine1;
    }

    public void setAddrLine1(String addrLine1) {
        this.addrLine1 = addrLine1;
    }

    public String getAddrLine2() {
        return addrLine2;
    }

    public void setAddrLine2(String addrLine2) {
        this.addrLine2 = addrLine2;
    }

    public String getAddrLine3() {
        return addrLine3;
    }

    public void setAddrLine3(String addrLine3) {
        this.addrLine3 = addrLine3;
    }

    public String getAddrStateCd() {
        return addrStateCd;
    }

    public void setAddrStateCd(String addrStateCd) {
        this.addrStateCd = addrStateCd;
    }

    public String getAddrCountryCd() {
        return addrCountryCd;
    }

    public void setAddrCountryCd(String addrCountryCd) {
        this.addrCountryCd = addrCountryCd;
    }

    public String getAddrZip() {
        return addrZip;
    }

    public void setAddrZip(String addrZip) {
        this.addrZip = addrZip;
    }

    public String getPhoneNum1() {
        return phoneNum1;
    }

    public void setPhoneNum1(String phoneNum1) {
        this.phoneNum1 = phoneNum1;
    }

    public String getPhoneNum2() {
        return phoneNum2;
    }

    public void setPhoneNum2(String phoneNum2) {
        this.phoneNum2 = phoneNum2;
    }

    public String getSsn() {
        return ssn;
    }

    public void setSsn(String ssn) {
        this.ssn = ssn;
    }

    public String getGovtIssuedId() {
        return govtIssuedId;
    }

    public void setGovtIssuedId(String govtIssuedId) {
        this.govtIssuedId = govtIssuedId;
    }

    public LocalDate getDob() {
        return dob;
    }

    public void setDob(LocalDate dob) {
        this.dob = dob;
    }

    public String getEftAccountId() {
        return eftAccountId;
    }

    public void setEftAccountId(String eftAccountId) {
        this.eftAccountId = eftAccountId;
    }

    public String getPriCardHolderInd() {
        return priCardHolderInd;
    }

    public void setPriCardHolderInd(String priCardHolderInd) {
        this.priCardHolderInd = priCardHolderInd;
    }

    public Integer getFicoCreditScore() {
        return ficoCreditScore;
    }

    public void setFicoCreditScore(Integer ficoCreditScore) {
        this.ficoCreditScore = ficoCreditScore;
    }

    public long getVersion() {
        return version;
    }
}
