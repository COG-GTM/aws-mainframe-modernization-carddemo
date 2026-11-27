package com.carddemo.data.entity;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OrderColumn;
import jakarta.persistence.Table;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Pending authorization summary per account (IMS HIDAM root segment PAUTSUM0 of DBPAUTP0).
 * <p>Source copybook: CIPAUSMY (optional IMS/DB2/MQ module).
 */
@Entity
@Table(name = "pending_auth_summary")
public class PendingAuthorizationSummary {

    /** PA-ACCT-ID PIC S9(11) COMP-3 - IMS root segment key (PAUTSUM0) */
    @Id
    @Column(name = "acct_id")
    private Long accountId;

    /** PA-CUST-ID PIC 9(09) */
    @Column(name = "cust_id")
    private Long customerId;

    /** PA-AUTH-STATUS PIC X(01) */
    @Column(name = "auth_status", length = 1)
    private String authStatus;

    /** PA-CREDIT-LIMIT PIC S9(09)V99 COMP-3 */
    @Column(name = "credit_limit", precision = 11, scale = 2)
    private BigDecimal creditLimit;

    /** PA-CASH-LIMIT PIC S9(09)V99 COMP-3 */
    @Column(name = "cash_limit", precision = 11, scale = 2)
    private BigDecimal cashLimit;

    /** PA-CREDIT-BALANCE PIC S9(09)V99 COMP-3 */
    @Column(name = "credit_balance", precision = 11, scale = 2)
    private BigDecimal creditBalance;

    /** PA-CASH-BALANCE PIC S9(09)V99 COMP-3 */
    @Column(name = "cash_balance", precision = 11, scale = 2)
    private BigDecimal cashBalance;

    /** PA-APPROVED-AUTH-CNT PIC S9(04) COMP */
    @Column(name = "approved_auth_cnt")
    private Integer approvedAuthCount;

    /** PA-DECLINED-AUTH-CNT PIC S9(04) COMP */
    @Column(name = "declined_auth_cnt")
    private Integer declinedAuthCount;

    /** PA-APPROVED-AUTH-AMT PIC S9(09)V99 COMP-3 */
    @Column(name = "approved_auth_amt", precision = 11, scale = 2)
    private BigDecimal approvedAuthAmount;

    /** PA-DECLINED-AUTH-AMT PIC S9(09)V99 COMP-3 */
    @Column(name = "declined_auth_amt", precision = 11, scale = 2)
    private BigDecimal declinedAuthAmount;

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public Long getCustomerId() {
        return customerId;
    }

    public void setCustomerId(Long customerId) {
        this.customerId = customerId;
    }

    public String getAuthStatus() {
        return authStatus;
    }

    public void setAuthStatus(String authStatus) {
        this.authStatus = authStatus;
    }

    public BigDecimal getCreditLimit() {
        return creditLimit;
    }

    public void setCreditLimit(BigDecimal creditLimit) {
        this.creditLimit = creditLimit;
    }

    public BigDecimal getCashLimit() {
        return cashLimit;
    }

    public void setCashLimit(BigDecimal cashLimit) {
        this.cashLimit = cashLimit;
    }

    public BigDecimal getCreditBalance() {
        return creditBalance;
    }

    public void setCreditBalance(BigDecimal creditBalance) {
        this.creditBalance = creditBalance;
    }

    public BigDecimal getCashBalance() {
        return cashBalance;
    }

    public void setCashBalance(BigDecimal cashBalance) {
        this.cashBalance = cashBalance;
    }

    public Integer getApprovedAuthCount() {
        return approvedAuthCount;
    }

    public void setApprovedAuthCount(Integer approvedAuthCount) {
        this.approvedAuthCount = approvedAuthCount;
    }

    public Integer getDeclinedAuthCount() {
        return declinedAuthCount;
    }

    public void setDeclinedAuthCount(Integer declinedAuthCount) {
        this.declinedAuthCount = declinedAuthCount;
    }

    public BigDecimal getApprovedAuthAmount() {
        return approvedAuthAmount;
    }

    public void setApprovedAuthAmount(BigDecimal approvedAuthAmount) {
        this.approvedAuthAmount = approvedAuthAmount;
    }

    public BigDecimal getDeclinedAuthAmount() {
        return declinedAuthAmount;
    }

    public void setDeclinedAuthAmount(BigDecimal declinedAuthAmount) {
        this.declinedAuthAmount = declinedAuthAmount;
    }

    /** PA-ACCOUNT-STATUS PIC X(02) OCCURS 5 TIMES, kept in order. */
    @ElementCollection
    @CollectionTable(name = "pending_auth_summary_status", joinColumns = @JoinColumn(name = "acct_id"))
    @OrderColumn(name = "status_index")
    @Column(name = "account_status", length = 2)
    private List<String> accountStatuses = new ArrayList<>();

    public List<String> getAccountStatuses() {
        return accountStatuses;
    }

    public void setAccountStatuses(List<String> accountStatuses) {
        this.accountStatuses = accountStatuses == null ? new ArrayList<>() : new ArrayList<>(accountStatuses);
    }
}
