package com.carddemo.card.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/** {@code card.card_xref}, CVACT03Y CARD-XREF-RECORD. Read-only for POSTTRAN (BND-007). */
@Entity
@Table(schema = "card", name = "card_xref")
public class CardXref {

    @Id
    @Column(name = "card_num", length = 16)
    private String cardNum;

    @Column(name = "cust_id", nullable = false)
    private long custId;

    @Column(name = "acct_id", nullable = false)
    private long acctId;

    protected CardXref() {
    }

    public CardXref(String cardNum, long custId, long acctId) {
        this.cardNum = cardNum;
        this.custId = custId;
        this.acctId = acctId;
    }

    public String getCardNum() { return cardNum; }
    public long getCustId() { return custId; }
    public long getAcctId() { return acctId; }
}
