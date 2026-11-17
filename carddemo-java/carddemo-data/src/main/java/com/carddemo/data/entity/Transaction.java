package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/** TRANSACT (copybook CVTRA05Y), VSAM KSDS keyed by TRAN-ID; AIX on TRAN-PROC-TS. */
@Entity
@Table(name = "transaction")
public class Transaction {

    @Id
    @Column(name = "tran_id", nullable = false, length = 16)
    private String tranId;

    @Column(name = "type_cd", nullable = false, length = 2)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private Short catCd;

    @Column(name = "source", nullable = false, length = 10)
    private String source;

    @Column(name = "description", nullable = false, length = 100)
    private String description;

    @Column(name = "amt", nullable = false, precision = 11, scale = 2)
    private BigDecimal amt;

    @Column(name = "merchant_id", nullable = false)
    private Integer merchantId;

    @Column(name = "merchant_name", nullable = false, length = 50)
    private String merchantName;

    @Column(name = "merchant_city", nullable = false, length = 50)
    private String merchantCity;

    @Column(name = "merchant_zip", nullable = false, length = 10)
    private String merchantZip;

    @Column(name = "card_num", nullable = false, length = 16)
    private String cardNum;

    @Column(name = "orig_ts")
    private LocalDateTime origTs;

    @Column(name = "proc_ts")
    private LocalDateTime procTs;

    public String getTranId() {
        return tranId;
    }

    public void setTranId(String tranId) {
        this.tranId = tranId;
    }

    public String getTypeCd() {
        return typeCd;
    }

    public void setTypeCd(String typeCd) {
        this.typeCd = typeCd;
    }

    public Short getCatCd() {
        return catCd;
    }

    public void setCatCd(Short catCd) {
        this.catCd = catCd;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public BigDecimal getAmt() {
        return amt;
    }

    public void setAmt(BigDecimal amt) {
        this.amt = amt;
    }

    public Integer getMerchantId() {
        return merchantId;
    }

    public void setMerchantId(Integer merchantId) {
        this.merchantId = merchantId;
    }

    public String getMerchantName() {
        return merchantName;
    }

    public void setMerchantName(String merchantName) {
        this.merchantName = merchantName;
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public void setMerchantCity(String merchantCity) {
        this.merchantCity = merchantCity;
    }

    public String getMerchantZip() {
        return merchantZip;
    }

    public void setMerchantZip(String merchantZip) {
        this.merchantZip = merchantZip;
    }

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String cardNum) {
        this.cardNum = cardNum;
    }

    public LocalDateTime getOrigTs() {
        return origTs;
    }

    public void setOrigTs(LocalDateTime origTs) {
        this.origTs = origTs;
    }

    public LocalDateTime getProcTs() {
        return procTs;
    }

    public void setProcTs(LocalDateTime procTs) {
        this.procTs = procTs;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof Transaction other && tranId != null && tranId.equals(other.getTranId()));
    }

    @Override
    public int hashCode() {
        return Transaction.class.hashCode();
    }
}
