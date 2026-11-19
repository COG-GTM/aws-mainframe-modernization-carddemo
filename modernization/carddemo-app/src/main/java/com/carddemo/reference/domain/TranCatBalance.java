package com.carddemo.reference.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import jakarta.persistence.Table;
import jakarta.persistence.Version;

import java.math.BigDecimal;

/** {@code reference.tran_cat_balance}, CVTRA01Y TRAN-CAT-BAL-RECORD. Optimistic lock on {@code version} (BND-006). */
@Entity
@Table(schema = "reference", name = "tran_cat_balance")
@IdClass(TranCatBalanceId.class)
public class TranCatBalance {

    @Id
    @Column(name = "acct_id")
    private Long acctId;

    @Id
    @Column(name = "type_cd", length = 2)
    private String typeCd;

    @Id
    @Column(name = "cat_cd")
    private Long catCd;

    @Column(name = "balance", precision = 11, scale = 2, nullable = false)
    private BigDecimal balance;

    @Version
    @Column(name = "version", nullable = false)
    private long version;

    protected TranCatBalance() {
    }

    public TranCatBalance(TranCatBalanceId id, BigDecimal balance) {
        this.acctId = id.acctId();
        this.typeCd = id.typeCd();
        this.catCd = id.catCd();
        this.balance = balance;
    }

    public TranCatBalanceId getId() { return new TranCatBalanceId(acctId, typeCd, catCd); }
    public BigDecimal getBalance() { return balance; }
    public void setBalance(BigDecimal balance) { this.balance = balance; }
    public long getVersion() { return version; }
}
