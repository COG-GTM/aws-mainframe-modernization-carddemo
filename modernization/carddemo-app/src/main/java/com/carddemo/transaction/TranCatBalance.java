package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;

/**
 * JPA entity for table {@code tran_cat_balance}. Transaction category balance record (TCATBALF KSDS, key TRAN-CAT-
 * KEY); written/rewritten by CBTRN02C, read sequentially by CBACT04C. Field Javadocs carry the CVTRA01Y item names;
 * {@link TranCatBalanceRecord} is the fixed-width value.
 */
@Entity
@Table(name = "tran_cat_balance")
public class TranCatBalance {

    @EmbeddedId
    private TranCatBalanceId id;

    /** TRAN-CAT-BAL PIC S9(09)V99. */
    @Column(name = "balance", precision = 11, scale = 2)
    private BigDecimal balance;

    protected TranCatBalance() {
    }

    public static TranCatBalance from(TranCatBalanceRecord record) {
        TranCatBalance entity = new TranCatBalance();
        entity.id = new TranCatBalanceId(record.acctId(), record.tranTypeCd(), record.tranCatCd());
        entity.balance = record.balance();
        return entity;
    }

    public TranCatBalanceRecord toRecord() {
        return new TranCatBalanceRecord(id.acctId(), id.tranTypeCd(), id.tranCatCd(), balance);
    }

    /** {@code REWRITE}: replaces every non-key field with the values of {@code record}; the key must match. */
    public void update(TranCatBalanceRecord record) {
        if (!id.equals(new TranCatBalanceId(record.acctId(), record.tranTypeCd(), record.tranCatCd()))) {
            throw new IllegalArgumentException("tran_cat_balance: cannot change the key of " + id + " via update");
        }
        this.balance = record.balance();
    }

    public TranCatBalanceId getId() {
        return id;
    }

    public BigDecimal getBalance() {
        return balance;
    }
}
