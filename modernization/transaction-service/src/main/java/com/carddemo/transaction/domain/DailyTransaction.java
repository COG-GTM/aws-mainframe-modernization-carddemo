package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DALYTRAN record, copybook CVTRA06Y: the unposted input of the POSTTRAN job. The mainframe read
 * it as a sequential file; here it is a staging table loaded by the same upstream feed.
 */
@Entity
@Table(name = "daily_transactions")
public class DailyTransaction {

    @Id
    @Column(name = "tran_id", length = 16, nullable = false)
    private String tranId;

    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private Integer catCd;

    @Column(name = "source", length = 10)
    private String source;

    @Column(name = "description", length = 100)
    private String description;

    @Column(name = "amount", precision = 11, scale = 2, nullable = false)
    private BigDecimal amount;

    @Column(name = "merchant_id")
    private Long merchantId;

    @Column(name = "merchant_name", length = 50)
    private String merchantName;

    @Column(name = "merchant_city", length = 50)
    private String merchantCity;

    @Column(name = "merchant_zip", length = 10)
    private String merchantZip;

    @Column(name = "card_num", length = 16, nullable = false)
    private String cardNum;

    @Column(name = "orig_ts")
    private LocalDateTime origTs;

    @Column(name = "proc_ts")
    private LocalDateTime procTs;

    @Column(name = "processed", nullable = false)
    private boolean processed;

    protected DailyTransaction() {
    }

    public DailyTransaction(String tranId, String cardNum, String typeCd, Integer catCd,
                            BigDecimal amount, LocalDateTime origTs) {
        this.tranId = tranId;
        this.cardNum = cardNum;
        this.typeCd = typeCd;
        this.catCd = catCd;
        this.amount = amount;
        this.origTs = origTs;
    }

    public String getTranId() {
        return tranId;
    }

    public String getTypeCd() {
        return typeCd;
    }

    public Integer getCatCd() {
        return catCd;
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

    public LocalDateTime getOrigTs() {
        return origTs;
    }

    public LocalDateTime getProcTs() {
        return procTs;
    }

    public void setProcTs(LocalDateTime procTs) {
        this.procTs = procTs;
    }

    public boolean isProcessed() {
        return processed;
    }

    public void setProcessed(boolean processed) {
        this.processed = processed;
    }
}
