package com.carddemo.card.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import java.time.LocalDate;

/** CARDDATA record, copybook CVACT02Y. */
@Entity
@Table(name = "cards")
public class Card {

    @Id
    @Column(name = "card_num", length = 16, nullable = false)
    private String cardNum;

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    @Column(name = "cvv_cd")
    private Integer cvvCd;

    @Column(name = "embossed_name", length = 50)
    private String embossedName;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @Column(name = "active_status", length = 1, nullable = false)
    private String activeStatus = "Y";

    @Version
    @Column(name = "version", nullable = false)
    private long version;

    protected Card() {
    }

    public Card(String cardNum, Long acctId) {
        this.cardNum = cardNum;
        this.acctId = acctId;
    }

    public String getCardNum() {
        return cardNum;
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

    public long getVersion() {
        return version;
    }
}
