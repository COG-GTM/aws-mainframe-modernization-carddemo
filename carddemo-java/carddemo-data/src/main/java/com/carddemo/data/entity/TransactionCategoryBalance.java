package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/** TCATBALF (copybook CVTRA01Y), VSAM KSDS keyed by TRAN-CAT-KEY (account, type, category). */
@Entity
@Table(name = "tran_cat_balance")
public class TransactionCategoryBalance {

    @EmbeddedId
    private TransactionCategoryBalanceId id;

    @Column(name = "balance", nullable = false, precision = 11, scale = 2)
    private BigDecimal balance;

    public TransactionCategoryBalanceId getId() {
        return id;
    }

    public void setId(TransactionCategoryBalanceId id) {
        this.id = id;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof TransactionCategoryBalance other && id != null && id.equals(other.getId()));
    }

    @Override
    public int hashCode() {
        return TransactionCategoryBalance.class.hashCode();
    }
}
