package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;
import java.util.Objects;

/** Card master record, CARDDATA (CVACT02Y). */
@Entity
@Table(name = "cards")
public class Card {

    @Id
    @Column(name = "card_num", nullable = false, length = 16)
    private String cardNum;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    @Column(name = "cvv_cd", nullable = false)
    private Integer cvvCd;

    @Column(name = "embossed_name", nullable = false, length = 50)
    private String embossedName;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @Column(name = "active_status", nullable = false, length = 1)
    private String activeStatus;

    public Card() {
    }

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String cardNum) {
        this.cardNum = cardNum;
    }

    public Long getAcctId() {
        return acctId;
    }

    public void setAcctId(Long acctId) {
        this.acctId = acctId;
    }

    public Integer getCvvCd() {
        return cvvCd;
    }

    public void setCvvCd(Integer cvvCd) {
        this.cvvCd = cvvCd;
    }

    public String getEmbossedName() {
        return embossedName;
    }

    public void setEmbossedName(String embossedName) {
        this.embossedName = embossedName;
    }

    public LocalDate getExpirationDate() {
        return expirationDate;
    }

    public void setExpirationDate(LocalDate expirationDate) {
        this.expirationDate = expirationDate;
    }

    public String getActiveStatus() {
        return activeStatus;
    }

    public void setActiveStatus(String activeStatus) {
        this.activeStatus = activeStatus;
    }

    @Override
    public boolean equals(Object other) {
        return this == other || other instanceof Card that && cardNum != null && cardNum.equals(that.cardNum);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(cardNum);
    }

    @Override
    public String toString() {
        return "Card[cardNum=" + cardNum + "]";
    }
}
