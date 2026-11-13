package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Table;

import java.time.LocalDate;

/**
 * Credit card (VSAM KSDS CARDDAT, RECLN 150).
 * <p>Source copybook: CVACT02Y.
 */
@Entity
@Table(name = "card", indexes = {
        @Index(name = "ix_card_acct_id", columnList = "acct_id")
})
public class Card {

    /** CARD-NUM PIC X(16) - VSAM KSDS key (CARDDAT) */
    @Id
    @Column(name = "card_num", length = 16)
    private String cardNumber;

    /** CARD-ACCT-ID PIC 9(11) - alternate index CARDAIX */
    @Column(name = "acct_id", nullable = false)
    private Long accountId;

    /** CARD-CVV-CD PIC 9(03) */
    @Column(name = "cvv_cd")
    private Integer cvvCode;

    /** CARD-EMBOSSED-NAME PIC X(50) */
    @Column(name = "embossed_name", length = 50)
    private String embossedName;

    /** CARD-EXPIRAION-DATE PIC X(10) (YYYY-MM-DD) */
    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    /** CARD-ACTIVE-STATUS PIC X(01) */
    @Column(name = "active_status", length = 1)
    private String activeStatus;

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }

    public Integer getCvvCode() {
        return cvvCode;
    }

    public void setCvvCode(Integer cvvCode) {
        this.cvvCode = cvvCode;
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
}
