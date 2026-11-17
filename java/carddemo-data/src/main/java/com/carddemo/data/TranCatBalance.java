package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.util.Objects;

/** Transaction category balance of an account, TCATBALF (CVTRA01Y). */
@Entity
@Table(name = "tran_cat_balances")
public class TranCatBalance {

    @EmbeddedId
    private TranCatBalanceId id;

    @Column(name = "balance", nullable = false, precision = 11, scale = 2)
    private BigDecimal balance;

    public TranCatBalance() {
    }

    public TranCatBalanceId getId() {
        return id;
    }

    public void setId(TranCatBalanceId id) {
        this.id = id;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    @Override
    public boolean equals(Object other) {
        return this == other || other instanceof TranCatBalance that && id != null && id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(id);
    }

    @Override
    public String toString() {
        return "TranCatBalance[id=" + id + "]";
    }
}
