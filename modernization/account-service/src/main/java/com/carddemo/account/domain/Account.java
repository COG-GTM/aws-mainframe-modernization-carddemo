package com.carddemo.account.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import java.math.BigDecimal;
import java.time.LocalDate;

/** ACCTDATA record, copybook CVACT01Y. */
@Entity
@Table(name = "accounts")
public class Account {

    @Id
    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    @Column(name = "active_status", length = 1, nullable = false)
    private String activeStatus;

    @Column(name = "curr_bal", precision = 12, scale = 2, nullable = false)
    private BigDecimal currBal = BigDecimal.ZERO;

    @Column(name = "credit_limit", precision = 12, scale = 2, nullable = false)
    private BigDecimal creditLimit = BigDecimal.ZERO;

    @Column(name = "cash_credit_limit", precision = 12, scale = 2, nullable = false)
    private BigDecimal cashCreditLimit = BigDecimal.ZERO;

    @Column(name = "open_date")
    private LocalDate openDate;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @Column(name = "reissue_date")
    private LocalDate reissueDate;

    @Column(name = "curr_cyc_credit", precision = 12, scale = 2, nullable = false)
    private BigDecimal currCycCredit = BigDecimal.ZERO;

    @Column(name = "curr_cyc_debit", precision = 12, scale = 2, nullable = false)
    private BigDecimal currCycDebit = BigDecimal.ZERO;

    @Column(name = "addr_zip", length = 10)
    private String addrZip;

    @Column(name = "group_id", length = 10)
    private String groupId;

    @Version
    @Column(name = "version", nullable = false)
    private long version;

    protected Account() {
    }

    public Account(Long acctId) {
        this.acctId = acctId;
        this.activeStatus = "Y";
    }

    public boolean isActive() {
        return "Y".equalsIgnoreCase(activeStatus);
    }

    /**
     * Available cycle headroom, {@code ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT} of
     * paragraph 1500-B-LOOKUP-ACCT in CBTRN02C.
     */
    public BigDecimal cycleBalance() {
        return currCycCredit.subtract(currCycDebit);
    }

    public Long getAcctId() {
        return acctId;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    public BigDecimal getCurrBal() {
        return currBal;
    }

    public void setCurrBal(BigDecimal currBal) {
        this.currBal = currBal;
    }

    public BigDecimal getCreditLimit() {
        return creditLimit;
    }

    public void setCreditLimit(BigDecimal creditLimit) {
        this.creditLimit = creditLimit;
    }

    public BigDecimal getCashCreditLimit() {
        return cashCreditLimit;
    }

    public void setCashCreditLimit(BigDecimal cashCreditLimit) {
        this.cashCreditLimit = cashCreditLimit;
    }

    public LocalDate getOpenDate() {
        return openDate;
    }

    public void setOpenDate(LocalDate openDate) {
        this.openDate = openDate;
    }

    public LocalDate getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(LocalDate expirationDate) {
        this.expirationDate = expirationDate;
    }

    public LocalDate getReissueDate() {
        return reissueDate;
    }

    public void setReissueDate(LocalDate reissueDate) {
        this.reissueDate = reissueDate;
    }

    public BigDecimal getCurrCycCredit() {
        return currCycCredit;
    }

    public void setCurrCycCredit(BigDecimal currCycCredit) {
        this.currCycCredit = currCycCredit;
    }

    public BigDecimal getCurrCycDebit() {
        return currCycDebit;
    }

    public void setCurrCycDebit(BigDecimal currCycDebit) {
        this.currCycDebit = currCycDebit;
    }

    public String getAddrZip() {
        return addrZip;
    }

    public void setAddrZip(String addrZip) {
        this.addrZip = addrZip;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public long getVersion() {
        return version;
    }
}
