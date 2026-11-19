package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.PostLoad;
import jakarta.persistence.PostPersist;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import org.springframework.data.domain.Persistable;

import java.math.BigDecimal;

/** TRANSACT record ({@code app/cpy/CVTRA05Y.cpy}); written once per posted DALYTRAN record. */
@Entity
@Table(schema = "transaction", name = "transactions")
public class Transaction implements Persistable<String> {

    @Id
    @Column(name = "tran_id", nullable = false)
    private String tranId;

    @Column(name = "type_cd", nullable = false)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private int catCd;

    @Column(name = "source", nullable = false)
    private String source;

    @Column(name = "description", nullable = false)
    private String description;

    @Column(name = "amount", nullable = false)
    private BigDecimal amount;

    @Column(name = "merchant_id", nullable = false)
    private long merchantId;

    @Column(name = "merchant_name", nullable = false)
    private String merchantName;

    @Column(name = "merchant_city", nullable = false)
    private String merchantCity;

    @Column(name = "merchant_zip", nullable = false)
    private String merchantZip;

    @Column(name = "card_num", nullable = false)
    private String cardNum;

    @Column(name = "orig_ts", nullable = false)
    private String origTs;

    @Column(name = "proc_ts", nullable = false)
    private String procTs;

    /** Zero with a negative sign (COBOL {@code '}'} overpunch), which NUMERIC cannot hold (D-052). */
    @Column(name = "amount_negative_zero", nullable = false)
    private boolean amountNegativeZero;

    /** New until loaded or persisted: {@code save} then INSERTs, so a duplicate key fails like a KSDS WRITE (status 22). */
    @Transient
    private boolean isNew = true;

    public Transaction() {
    }

    @Override
    public String getId() {
        return tranId;
    }

    @Override
    public boolean isNew() {
        return isNew;
    }

    @PostLoad
    @PostPersist
    void markNotNew() {
        this.isNew = false;
    }

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

    public int getCatCd() {
        return catCd;
    }

    public void setCatCd(int catCd) {
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

    public long getMerchantId() {
        return merchantId;
    }

    public void setMerchantId(long merchantId) {
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

    public String getOrigTs() {
        return origTs;
    }

    public void setOrigTs(String origTs) {
        this.origTs = origTs;
    }

    public String getProcTs() {
        return procTs;
    }

    public void setProcTs(String procTs) {
        this.procTs = procTs;
    }

    public boolean isAmountNegativeZero() {
        return amountNegativeZero;
    }

    public void setAmountNegativeZero(boolean amountNegativeZero) {
        this.amountNegativeZero = amountNegativeZero;
    }
}
