package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.time.LocalDate;

/** CARDDATA / CARDDAT (copybook CVACT02Y), VSAM KSDS keyed by CARD-NUM; AIX CARDAIX on CARD-ACCT-ID. */
@Entity
@Table(name = "card")
public class Card {

    @Id
    @Column(name = "card_num", nullable = false, length = 16)
    private String cardNum;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    @Column(name = "cvv_cd", nullable = false)
    private Short cvvCd;

    @Column(name = "embossed_name", nullable = false, length = 50)
    private String embossedName;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @Column(name = "active_status", nullable = false, length = 1)
    private String activeStatus;

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

    public Short getCvvCd() {
        return cvvCd;
    }

    public void setCvvCd(Short cvvCd) {
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
    public boolean equals(Object o) {
        return this == o || (o instanceof Card other && cardNum != null && cardNum.equals(other.getCardNum()));
    }

    @Override
    public int hashCode() {
        return Card.class.hashCode();
    }
}
