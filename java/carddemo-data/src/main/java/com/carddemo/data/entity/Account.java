package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Credit card account master (VSAM KSDS ACCTDAT, RECLN 300).
 * <p>Source copybook: CVACT01Y.
 */
@Entity
@Table(name = "account")
public class Account {

    /** ACCT-ID PIC 9(11) - VSAM KSDS key (ACCTDAT) */
    @Id
    @Column(name = "acct_id")
    private Long id;

    /** ACCT-ACTIVE-STATUS PIC X(01) */
    @Column(name = "active_status", length = 1)
    private String activeStatus;

    /** ACCT-CURR-BAL PIC S9(10)V99 */
    @Column(name = "curr_bal", precision = 12, scale = 2)
    private BigDecimal currentBalance;

    /** ACCT-CREDIT-LIMIT PIC S9(10)V99 */
    @Column(name = "credit_limit", precision = 12, scale = 2)
    private BigDecimal creditLimit;

    /** ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99 */
    @Column(name = "cash_credit_limit", precision = 12, scale = 2)
    private BigDecimal cashCreditLimit;

    /** ACCT-OPEN-DATE PIC X(10) (YYYY-MM-DD) */
    @Column(name = "open_date")
    private LocalDate openDate;

    /** ACCT-EXPIRAION-DATE PIC X(10) (YYYY-MM-DD) */
    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    /** ACCT-REISSUE-DATE PIC X(10) (YYYY-MM-DD) */
    @Column(name = "reissue_date")
    private LocalDate reissueDate;

    /** ACCT-CURR-CYC-CREDIT PIC S9(10)V99 */
    @Column(name = "curr_cyc_credit", precision = 12, scale = 2)
    private BigDecimal currentCycleCredit;

    /** ACCT-CURR-CYC-DEBIT PIC S9(10)V99 */
    @Column(name = "curr_cyc_debit", precision = 12, scale = 2)
    private BigDecimal currentCycleDebit;

    /** ACCT-ADDR-ZIP PIC X(10) */
    @Column(name = "addr_zip", length = 10)
    private String addressZip;

    /** ACCT-GROUP-ID PIC X(10) */
    @Column(name = "group_id", length = 10)
    private String groupId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    public BigDecimal getCurrentBalance() {
        return currentBalance;
    }

    public void setCurrentBalance(BigDecimal currentBalance) {
        this.currentBalance = currentBalance;
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

    public BigDecimal getCurrentCycleCredit() {
        return currentCycleCredit;
    }

    public void setCurrentCycleCredit(BigDecimal currentCycleCredit) {
        this.currentCycleCredit = currentCycleCredit;
    }

    public BigDecimal getCurrentCycleDebit() {
        return currentCycleDebit;
    }

    public void setCurrentCycleDebit(BigDecimal currentCycleDebit) {
        this.currentCycleDebit = currentCycleDebit;
    }

    public String getAddressZip() {
        return addressZip;
    }

    public void setAddressZip(String addressZip) {
        this.addressZip = addressZip;
    }

    public String getGroupId() {
        return groupId;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }
}
