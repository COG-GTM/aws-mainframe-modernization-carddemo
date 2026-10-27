package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/**
 * ACCOUNT-RECORD (copybook CVACT01Y, RECLN 300) - ACCTDATA VSAM KSDS keyed by ACCT-ID.
 */
@Entity
@Table(name = "ACCTDATA")
public class Account {

    @Id
    @Column(name = "ACCT_ID")
    private Long acctId;                      // PIC 9(11)
    @Column(name = "ACCT_ACTIVE_STATUS", length = 1)
    private String activeStatus;              // PIC X(01)
    @Column(name = "ACCT_CURR_BAL", precision = 12, scale = 2)
    private BigDecimal currBal;               // PIC S9(10)V99
    @Column(name = "ACCT_CREDIT_LIMIT", precision = 12, scale = 2)
    private BigDecimal creditLimit;           // PIC S9(10)V99
    @Column(name = "ACCT_CASH_CREDIT_LIMIT", precision = 12, scale = 2)
    private BigDecimal cashCreditLimit;       // PIC S9(10)V99
    @Column(name = "ACCT_OPEN_DATE", length = 10)
    private String openDate;                  // PIC X(10)
    @Column(name = "ACCT_EXPIRAION_DATE", length = 10)
    private String expirationDate;            // PIC X(10) (sic: ACCT-EXPIRAION-DATE)
    @Column(name = "ACCT_REISSUE_DATE", length = 10)
    private String reissueDate;               // PIC X(10)
    @Column(name = "ACCT_CURR_CYC_CREDIT", precision = 12, scale = 2)
    private BigDecimal currCycCredit;         // PIC S9(10)V99
    @Column(name = "ACCT_CURR_CYC_DEBIT", precision = 12, scale = 2)
    private BigDecimal currCycDebit;          // PIC S9(10)V99
    @Column(name = "ACCT_ADDR_ZIP", length = 10)
    private String addrZip;                   // PIC X(10)
    @Column(name = "ACCT_GROUP_ID", length = 10)
    private String groupId;                   // PIC X(10)

    public Long getAcctId() { return acctId; }
    public void setAcctId(long v) { this.acctId = Pic.n9(v, 11); }
    public String getActiveStatus() { return activeStatus; }
    public void setActiveStatus(String v) { this.activeStatus = Pic.x(v, 1); }
    public BigDecimal getCurrBal() { return currBal; }
    public void setCurrBal(BigDecimal v) { this.currBal = Pic.s9(v, 10, 2); }
    public BigDecimal getCreditLimit() { return creditLimit; }
    public void setCreditLimit(BigDecimal v) { this.creditLimit = Pic.s9(v, 10, 2); }
    public BigDecimal getCashCreditLimit() { return cashCreditLimit; }
    public void setCashCreditLimit(BigDecimal v) { this.cashCreditLimit = Pic.s9(v, 10, 2); }
    public String getOpenDate() { return openDate; }
    public void setOpenDate(String v) { this.openDate = Pic.x(v, 10); }
    public String getExpirationDate() { return expirationDate; }
    public void setExpirationDate(String v) { this.expirationDate = Pic.x(v, 10); }
    public String getReissueDate() { return reissueDate; }
    public void setReissueDate(String v) { this.reissueDate = Pic.x(v, 10); }
    public BigDecimal getCurrCycCredit() { return currCycCredit; }
    public void setCurrCycCredit(BigDecimal v) { this.currCycCredit = Pic.s9(v, 10, 2); }
    public BigDecimal getCurrCycDebit() { return currCycDebit; }
    public void setCurrCycDebit(BigDecimal v) { this.currCycDebit = Pic.s9(v, 10, 2); }
    public String getAddrZip() { return addrZip; }
    public void setAddrZip(String v) { this.addrZip = Pic.x(v, 10); }
    public String getGroupId() { return groupId; }
    public void setGroupId(String v) { this.groupId = Pic.x(v, 10); }
}
