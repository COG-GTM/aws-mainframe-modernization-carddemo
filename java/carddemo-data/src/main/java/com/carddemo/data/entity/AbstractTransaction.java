package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Id;
import jakarta.persistence.MappedSuperclass;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/** Columns shared by the posted transaction (CVTRA05Y) and daily transaction (CVTRA06Y) layouts, which are identical. */
@MappedSuperclass
public abstract class AbstractTransaction {

    /** TRAN-ID PIC X(16) */
    @Id
    @Column(name = "tran_id", length = 16)
    private String id;

    /** TRAN-TYPE-CD PIC X(02) */
    @Column(name = "type_cd", length = 2)
    private String typeCode;

    /** TRAN-CAT-CD PIC 9(04) */
    @Column(name = "cat_cd")
    private Integer categoryCode;

    /** TRAN-SOURCE PIC X(10) */
    @Column(name = "source", length = 10)
    private String source;

    /** TRAN-DESC PIC X(100) */
    @Column(name = "description", length = 100)
    private String description;

    /** TRAN-AMT PIC S9(09)V99 */
    @Column(name = "amount", precision = 11, scale = 2)
    private BigDecimal amount;

    /** TRAN-MERCHANT-ID PIC 9(09) */
    @Column(name = "merchant_id")
    private Long merchantId;

    /** TRAN-MERCHANT-NAME PIC X(50) */
    @Column(name = "merchant_name", length = 50)
    private String merchantName;

    /** TRAN-MERCHANT-CITY PIC X(50) */
    @Column(name = "merchant_city", length = 50)
    private String merchantCity;

    /** TRAN-MERCHANT-ZIP PIC X(10) */
    @Column(name = "merchant_zip", length = 10)
    private String merchantZip;

    /** TRAN-CARD-NUM PIC X(16) */
    @Column(name = "card_num", length = 16)
    private String cardNumber;

    /** TRAN-ORIG-TS PIC X(26) (YYYY-MM-DD HH:MM:SS.ffffff) */
    @Column(name = "orig_ts")
    private LocalDateTime originTimestamp;

    /** TRAN-PROC-TS PIC X(26) (YYYY-MM-DD HH:MM:SS.ffffff) */
    @Column(name = "proc_ts")
    private LocalDateTime processedTimestamp;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Integer getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(Integer categoryCode) {
        this.categoryCode = categoryCode;
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

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public LocalDateTime getOriginTimestamp() {
        return originTimestamp;
    }

    public void setOriginTimestamp(LocalDateTime originTimestamp) {
        this.originTimestamp = originTimestamp;
    }

    public LocalDateTime getProcessedTimestamp() {
        return processedTimestamp;
    }

    public void setProcessedTimestamp(LocalDateTime processedTimestamp) {
        this.processedTimestamp = processedTimestamp;
    }
}
