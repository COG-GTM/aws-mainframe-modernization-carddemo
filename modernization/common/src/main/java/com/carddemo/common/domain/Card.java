package com.carddemo.common.domain;

import jakarta.persistence.*;
import java.time.LocalDate;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table card; legacy source CARDDAT / CVACT02Y (AIX CARDAIX on acct_id). */
@Entity
@Table(name = "card")
public class Card {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "card_num")
    private String cardNum;

    @Column(name = "acct_id")
    private Long acctId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "cvv_cd")
    private String cvvCd;

    @Column(name = "embossed_name")
    private String embossedName;

    @Column(name = "expiration_date")
    private LocalDate expirationDate;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "active_status")
    private String activeStatus;

    @Version
    @Column(name = "version")
    private long version;

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

    public String getCvvCd() {
        return cvvCd;
    }

    public void setCvvCd(String cvvCd) {
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
