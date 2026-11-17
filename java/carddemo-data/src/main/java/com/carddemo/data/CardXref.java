package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.Objects;

/** Card to customer and account cross-reference, CARDXREF (CVACT03Y). */
@Entity
@Table(name = "card_xrefs")
public class CardXref {

    @Id
    @Column(name = "card_num", nullable = false, length = 16)
    private String cardNum;

    @Column(name = "cust_id", nullable = false)
    private Long custId;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    public CardXref() {
    }

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String cardNum) {
        this.cardNum = cardNum;
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

    @Override
    public boolean equals(Object other) {
        return this == other || other instanceof CardXref that && cardNum != null && cardNum.equals(that.cardNum);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(cardNum);
    }

    @Override
    public String toString() {
        return "CardXref[cardNum=" + cardNum + "]";
    }
}
