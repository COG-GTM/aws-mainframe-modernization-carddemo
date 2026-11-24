package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 * CUSTOMER-RECORD (copybook CVCUS01Y, RECLN 500) - CUSTDATA VSAM KSDS keyed by CUST-ID.
 */
@Entity
@Table(name = "CUSTDATA")
public class Customer {

    @Id
    @Column(name = "CUST_ID")
    private Long custId;                    // PIC 9(09)
    @Column(name = "CUST_FIRST_NAME", length = 25)
    private String firstName;
    @Column(name = "CUST_MIDDLE_NAME", length = 25)
    private String middleName;
    @Column(name = "CUST_LAST_NAME", length = 25)
    private String lastName;
    @Column(name = "CUST_ADDR_LINE_1", length = 50)
    private String addrLine1;
    @Column(name = "CUST_ADDR_LINE_2", length = 50)
    private String addrLine2;
    @Column(name = "CUST_ADDR_LINE_3", length = 50)
    private String addrLine3;
    @Column(name = "CUST_ADDR_STATE_CD", length = 2)
    private String addrStateCd;
    @Column(name = "CUST_ADDR_COUNTRY_CD", length = 3)
    private String addrCountryCd;
    @Column(name = "CUST_ADDR_ZIP", length = 10)
    private String addrZip;
    @Column(name = "CUST_PHONE_NUM_1", length = 15)
    private String phoneNum1;
    @Column(name = "CUST_PHONE_NUM_2", length = 15)
    private String phoneNum2;
    @Column(name = "CUST_SSN")
    private Long ssn;                       // PIC 9(09)
    @Column(name = "CUST_GOVT_ISSUED_ID", length = 20)
    private String govtIssuedId;
    @Column(name = "CUST_DOB_YYYY_MM_DD", length = 10)
    private String dob;
    @Column(name = "CUST_EFT_ACCOUNT_ID", length = 10)
    private String eftAccountId;
    @Column(name = "CUST_PRI_CARD_HOLDER_IND", length = 1)
    private String priCardHolderInd;
    @Column(name = "CUST_FICO_CREDIT_SCORE")
    private Integer ficoCreditScore;        // PIC 9(03)

    public Long getCustId() { return custId; }
    public void setCustId(long v) { this.custId = Pic.n9(v, 9); }
    public String getFirstName() { return firstName; }
    public void setFirstName(String v) { this.firstName = Pic.x(v, 25); }
    public String getMiddleName() { return middleName; }
    public void setMiddleName(String v) { this.middleName = Pic.x(v, 25); }
    public String getLastName() { return lastName; }
    public void setLastName(String v) { this.lastName = Pic.x(v, 25); }
    public String getAddrLine1() { return addrLine1; }
    public void setAddrLine1(String v) { this.addrLine1 = Pic.x(v, 50); }
    public String getAddrLine2() { return addrLine2; }
    public void setAddrLine2(String v) { this.addrLine2 = Pic.x(v, 50); }
    public String getAddrLine3() { return addrLine3; }
    public void setAddrLine3(String v) { this.addrLine3 = Pic.x(v, 50); }
    public String getAddrStateCd() { return addrStateCd; }
    public void setAddrStateCd(String v) { this.addrStateCd = Pic.x(v, 2); }
    public String getAddrCountryCd() { return addrCountryCd; }
    public void setAddrCountryCd(String v) { this.addrCountryCd = Pic.x(v, 3); }
    public String getAddrZip() { return addrZip; }
    public void setAddrZip(String v) { this.addrZip = Pic.x(v, 10); }
    public String getPhoneNum1() { return phoneNum1; }
    public void setPhoneNum1(String v) { this.phoneNum1 = Pic.x(v, 15); }
    public String getPhoneNum2() { return phoneNum2; }
    public void setPhoneNum2(String v) { this.phoneNum2 = Pic.x(v, 15); }
    public Long getSsn() { return ssn; }
    public void setSsn(long v) { this.ssn = Pic.n9(v, 9); }
    public String getGovtIssuedId() { return govtIssuedId; }
    public void setGovtIssuedId(String v) { this.govtIssuedId = Pic.x(v, 20); }
    public String getDob() { return dob; }
    public void setDob(String v) { this.dob = Pic.x(v, 10); }
    public String getEftAccountId() { return eftAccountId; }
    public void setEftAccountId(String v) { this.eftAccountId = Pic.x(v, 10); }
    public String getPriCardHolderInd() { return priCardHolderInd; }
    public void setPriCardHolderInd(String v) { this.priCardHolderInd = Pic.x(v, 1); }
    public Integer getFicoCreditScore() { return ficoCreditScore; }
    public void setFicoCreditScore(int v) { this.ficoCreditScore = Pic.n9(v, 3); }
}
