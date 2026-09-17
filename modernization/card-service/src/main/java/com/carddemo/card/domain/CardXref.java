package com.carddemo.card.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 * CARDXREF record, copybook CVACT03Y: the card to customer and account resolution used by the
 * online account view and by the CBTRN02C / CBACT04C batch programs.
 */
@Entity
@Table(name = "card_xref")
public class CardXref {

    @Id
    @Column(name = "card_num", length = 16, nullable = false)
    private String cardNum;

    @Column(name = "cust_id", nullable = false)
    private Long custId;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    protected CardXref() {
    }

    public CardXref(String cardNum, Long custId, Long acctId) {
        this.cardNum = cardNum;
        this.custId = custId;
        this.acctId = acctId;
    }

    public String getCardNum() {
        return cardNum;
    }

    public Long getCustId() {
        return custId;
    }

    public void setCustId(Long custId) {
        this.custId = custId;
    }

    public Long getAcctId() {
        return acctId;
    }

    public void setAcctId(Long acctId) {
        this.acctId = acctId;
    }
}
