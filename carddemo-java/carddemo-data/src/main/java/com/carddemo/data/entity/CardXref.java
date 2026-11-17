package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/** CARDXREF / CCXREF (copybook CVACT03Y), VSAM KSDS keyed by XREF-CARD-NUM; AIX CXACAIX on XREF-ACCT-ID. */
@Entity
@Table(name = "card_xref")
public class CardXref {

    @Id
    @Column(name = "card_num", nullable = false, length = 16)
    private String cardNum;

    @Column(name = "cust_id", nullable = false)
    private Integer custId;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String cardNum) {
        this.cardNum = cardNum;
    }

    public Integer getCustId() {
        return custId;
    }

    public void setCustId(Integer custId) {
        this.custId = custId;
    }

    public Long getAcctId() {
        return acctId;
    }

    public void setAcctId(Long acctId) {
        this.acctId = acctId;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof CardXref other && cardNum != null && cardNum.equals(other.getCardNum()));
    }

    @Override
    public int hashCode() {
        return CardXref.class.hashCode();
    }
}
