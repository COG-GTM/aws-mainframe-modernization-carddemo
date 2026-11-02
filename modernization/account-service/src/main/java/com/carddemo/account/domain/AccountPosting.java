package com.carddemo.account.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.PostLoad;
import jakarta.persistence.PostPersist;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.springframework.data.domain.Persistable;

/**
 * Idempotency record of one applied posting, keyed by the transaction id ({@code DALYTRAN-ID} for
 * POSTTRAN). Holds the account figures right after the posting so a replay returns the same result.
 */
@Entity
@Table(name = "account_postings")
public class AccountPosting implements Persistable<String> {

    @Id
    @Column(name = "tran_id", length = 16)
    private String tranId;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    @Column(name = "amount", nullable = false, precision = 12, scale = 2)
    private BigDecimal amount;

    @Column(name = "transaction_date", nullable = false)
    private LocalDate transactionDate;

    @Column(name = "curr_bal", nullable = false, precision = 12, scale = 2)
    private BigDecimal currBal;

    @Column(name = "curr_cyc_credit", nullable = false, precision = 12, scale = 2)
    private BigDecimal currCycCredit;

    @Column(name = "curr_cyc_debit", nullable = false, precision = 12, scale = 2)
    private BigDecimal currCycDebit;

    @Column(name = "posted_at", nullable = false)
    private LocalDateTime postedAt;

    /** Always INSERT a new posting, so a concurrent one with the same id fails on the key instead of being merged over. */
    @Transient
    private boolean newPosting = true;

    protected AccountPosting() {
    }

    @Override
    public String getId() {
        return tranId;
    }

    @Override
    public boolean isNew() {
        return newPosting;
    }

    @PostLoad
    @PostPersist
    void markStored() {
        newPosting = false;
    }

    public AccountPosting(String tranId, Account account, BigDecimal amount, LocalDate transactionDate,
                          LocalDateTime postedAt) {
        this.tranId = tranId;
        this.acctId = account.getAcctId();
        this.amount = amount;
        this.transactionDate = transactionDate;
        this.currBal = account.getCurrBal();
        this.currCycCredit = account.getCurrCycCredit();
        this.currCycDebit = account.getCurrCycDebit();
        this.postedAt = postedAt;
    }

    public String getTranId() {
        return tranId;
    }

    public Long getAcctId() {
        return acctId;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public LocalDate getTransactionDate() {
        return transactionDate;
    }

    public BigDecimal getCurrBal() {
        return currBal;
    }

    public BigDecimal getCurrCycCredit() {
        return currCycCredit;
    }

    public BigDecimal getCurrCycDebit() {
        return currCycDebit;
    }

    public LocalDateTime getPostedAt() {
        return postedAt;
    }
}
