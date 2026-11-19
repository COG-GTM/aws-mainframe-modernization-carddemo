package com.carddemo.card.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.PostLoad;
import jakarta.persistence.PostPersist;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import org.springframework.data.domain.Persistable;

/** CARDXREF record ({@code app/cpy/CVACT03Y.cpy}); read-only for POSTTRAN. */
@Entity
@Table(schema = "card", name = "card_xref")
public class CardXref implements Persistable<String> {

    @Id
    @Column(name = "card_num", nullable = false)
    private String cardNum;

    @Column(name = "cust_id", nullable = false)
    private long custId;

    @Column(name = "acct_id", nullable = false)
    private long acctId;

    /** New until loaded or persisted: {@code save} then INSERTs, so a duplicate key fails like a KSDS WRITE (status 22). */
    @Transient
    private boolean isNew = true;

    public CardXref() {
    }

    @Override
    public String getId() {
        return cardNum;
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

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String cardNum) {
        this.cardNum = cardNum;
    }

    public long getCustId() {
        return custId;
    }

    public void setCustId(long custId) {
        this.custId = custId;
    }

    public long getAcctId() {
        return acctId;
    }

    public void setAcctId(long acctId) {
        this.acctId = acctId;
    }
}
