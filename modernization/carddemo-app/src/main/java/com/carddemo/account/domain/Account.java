package com.carddemo.account.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.PostLoad;
import jakarta.persistence.PostPersist;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.Version;
import org.springframework.data.domain.Persistable;

import java.math.BigDecimal;

/** ACCTDATA record ({@code app/cpy/CVACT01Y.cpy}); rewritten by POSTTRAN, hence {@code @Version} (D-019). */
@Entity
@Table(schema = "account", name = "accounts")
public class Account implements Persistable<Long> {

    @Id
    @Column(name = "acct_id", nullable = false)
    private long acctId;

    @Column(name = "active_status", nullable = false)
    private String activeStatus;

    @Column(name = "curr_bal", nullable = false)
    private BigDecimal currBal;

    @Column(name = "credit_limit", nullable = false)
    private BigDecimal creditLimit;

    @Column(name = "cash_credit_limit", nullable = false)
    private BigDecimal cashCreditLimit;

    @Column(name = "open_date", nullable = false)
    private String openDate;

    @Column(name = "expiration_date", nullable = false)
    private String expirationDate;

    @Column(name = "reissue_date", nullable = false)
    private String reissueDate;

    @Column(name = "curr_cyc_credit", nullable = false)
    private BigDecimal currCycCredit;

    @Column(name = "curr_cyc_debit", nullable = false)
    private BigDecimal currCycDebit;

    @Column(name = "addr_zip", nullable = false)
    private String addrZip;

    @Column(name = "group_id", nullable = false)
    private String groupId;

    @Version
    @Column(name = "version", nullable = false)
    private long version;

    /** New until loaded or persisted: {@code save} then INSERTs, so a duplicate key fails like a KSDS WRITE (status 22). */
    @Transient
    private boolean isNew = true;

    public Account() {
    }

    @Override
    public Long getId() {
        return acctId;
    }

    @Override
    public boolean isNew() {
        return isNew;
    }

    @PostLoad
    @PostPersist
    void markNotNew() {
        this.isNew = false;
    }

    public long getAcctId() {
        return acctId;
    }

    public void setAcctId(long acctId) {
        this.acctId = acctId;
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

    public String getOpenDate() {
        return openDate;
    }

    public void setOpenDate(String openDate) {
        this.openDate = openDate;
    }

    public String getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(String expirationDate) {
        this.expirationDate = expirationDate;
    }

    public String getReissueDate() {
        return reissueDate;
    }

    public void setReissueDate(String reissueDate) {
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
