package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;

/** TCATBAL record, copybook CVTRA01Y: the per category balance updated by POSTTRAN. */
@Entity
@Table(name = "category_balances")
public class CategoryBalance {

    @EmbeddedId
    private CategoryBalanceId id;

    @Column(name = "balance", precision = 11, scale = 2, nullable = false)
    private BigDecimal balance = BigDecimal.ZERO;

    protected CategoryBalance() {
    }

    public CategoryBalance(CategoryBalanceId id, BigDecimal balance) {
        this.id = id;
        this.balance = balance;
    }

    public CategoryBalanceId getId() {
        return id;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void add(BigDecimal amount) {
        this.balance = this.balance.add(amount);
    }
}
