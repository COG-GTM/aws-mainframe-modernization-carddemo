package com.carddemo.common.domain;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table card_transaction; legacy source TRANSACT / CVTRA05Y (AIX on proc_ts). */
@Entity
@Table(name = "card_transaction")
public class CardTransaction {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "tran_id")
    private String tranId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "type_cd")
    private String typeCd;

    @Column(name = "cat_cd")
    private Integer catCd;

    @Column(name = "source")
    private String source;

    @Column(name = "description")
    private String description;

    @Column(name = "amount")
    private BigDecimal amount;

    @Column(name = "merchant_id")
    private Long merchantId;

    @Column(name = "merchant_name")
    private String merchantName;

    @Column(name = "merchant_city")
    private String merchantCity;

    @Column(name = "merchant_zip")
    private String merchantZip;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "card_num")
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

    public Integer getCatCd() {
        return catCd;
    }

    public void setCatCd(Integer catCd) {
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

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public Long getMerchantId() {
        return merchantId;
    }

    public void setMerchantId(Long merchantId) {
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
}
