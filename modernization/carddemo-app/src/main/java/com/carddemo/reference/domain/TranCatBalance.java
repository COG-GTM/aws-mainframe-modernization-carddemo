package com.carddemo.reference.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import jakarta.persistence.PostLoad;
import jakarta.persistence.PostPersist;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.Version;
import org.springframework.data.domain.Persistable;

import java.math.BigDecimal;

/** TCATBALF record ({@code app/cpy/CVTRA01Y.cpy}); key TRAN-CAT-KEY; rewritten by POSTTRAN, hence {@code @Version} (D-019). */
@Entity
@Table(schema = "reference", name = "tran_cat_balances")
@IdClass(TranCatBalanceId.class)
public class TranCatBalance implements Persistable<TranCatBalanceId> {

    @Id
    @Column(name = "acct_id", nullable = false)
    private long acctId;

    @Id
    @Column(name = "type_cd", nullable = false)
    private String typeCd;

    @Id
    @Column(name = "cat_cd", nullable = false)
    private int catCd;

    @Column(name = "balance", nullable = false)
    private BigDecimal balance;

    /** Zero with a negative sign (COBOL {@code '}'} overpunch), which NUMERIC cannot hold (D-052). */
    @Column(name = "balance_negative_zero", nullable = false)
    private boolean balanceNegativeZero;

    @Version
    @Column(name = "version", nullable = false)
    private long version;

    /** New until loaded or persisted: {@code save} then INSERTs, so a duplicate key fails like a KSDS WRITE (status 22). */
    @Transient
    private boolean isNew = true;

    public TranCatBalance() {
    }

    @Override
    public TranCatBalanceId getId() {
        return new TranCatBalanceId(acctId, typeCd, catCd);
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

    public String getTypeCd() {
        return typeCd;
    }

    public void setTypeCd(String typeCd) {
        this.typeCd = typeCd;
    }

    public int getCatCd() {
        return catCd;
    }

    public void setCatCd(int catCd) {
        this.catCd = catCd;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    public long getVersion() {
        return version;
    }

    public boolean isBalanceNegativeZero() {
        return balanceNegativeZero;
    }

    public void setBalanceNegativeZero(boolean balanceNegativeZero) {
        this.balanceNegativeZero = balanceNegativeZero;
    }
}
