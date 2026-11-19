package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * {@code transaction.transaction}, CVTRA05Y TRAN-RECORD. Append-only for POSTTRAN (BND-004), so no {@code @Version}.
 * Alphanumeric fields are stored without trailing spaces; the timestamp texts are stored as is (D-015).
 */
@Entity
@Table(schema = "transaction", name = "transaction")
public class Transaction {

    @Id
    @Column(name = "tran_id", length = 16)
    private String tranId;

    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private long catCd;

    @Column(name = "source", length = 10, nullable = false)
    private String source;

    @Column(name = "description", length = 100, nullable = false)
    private String description;

    @Column(name = "amount", precision = 11, scale = 2, nullable = false)
    private BigDecimal amount;

    @Column(name = "merchant_id", nullable = false)
    private long merchantId;

    @Column(name = "merchant_name", length = 50, nullable = false)
    private String merchantName;

    @Column(name = "merchant_city", length = 50, nullable = false)
    private String merchantCity;

    @Column(name = "merchant_zip", length = 10, nullable = false)
    private String merchantZip;

    @Column(name = "card_num", length = 16, nullable = false)
    private String cardNum;

    @Column(name = "orig_ts")
    private LocalDateTime origTs;

    @Column(name = "orig_ts_text", length = TransactionTimestamp.LENGTH, nullable = false)
    private String origTsText;

    @Column(name = "proc_ts")
    private LocalDateTime procTs;

    @Column(name = "proc_ts_text", length = TransactionTimestamp.LENGTH, nullable = false)
    private String procTsText;

    protected Transaction() {
    }

    public Transaction(String tranId) {
        this.tranId = Objects.requireNonNull(tranId);
    }

    public String getTranId() { return tranId; }
    public String getTypeCd() { return typeCd; }
    public void setTypeCd(String typeCd) { this.typeCd = typeCd; }
    public long getCatCd() { return catCd; }
    public void setCatCd(long catCd) { this.catCd = catCd; }
    public String getSource() { return source; }
    public void setSource(String source) { this.source = source; }
    public String getDescription() { return description; }
    public void setDescription(String description) { this.description = description; }
    public BigDecimal getAmount() { return amount; }
    public void setAmount(BigDecimal amount) { this.amount = amount; }
    public long getMerchantId() { return merchantId; }
    public void setMerchantId(long merchantId) { this.merchantId = merchantId; }
    public String getMerchantName() { return merchantName; }
    public void setMerchantName(String merchantName) { this.merchantName = merchantName; }
    public String getMerchantCity() { return merchantCity; }
    public void setMerchantCity(String merchantCity) { this.merchantCity = merchantCity; }
    public String getMerchantZip() { return merchantZip; }
    public void setMerchantZip(String merchantZip) { this.merchantZip = merchantZip; }
    public String getCardNum() { return cardNum; }
    public void setCardNum(String cardNum) { this.cardNum = cardNum; }
    public LocalDateTime getOrigTs() { return origTs; }
    public String getOrigTsText() { return origTsText; }
    public LocalDateTime getProcTs() { return procTs; }
    public String getProcTsText() { return procTsText; }

    /** Sets TRAN-ORIG-TS from its 26-byte text; the parsed value is null if the text is in neither layout. */
    public void setOrigTsText(String text) {
        this.origTsText = requireTimestampText(text);
        this.origTs = TransactionTimestamp.parse(text).orElse(null);
    }

    /** Sets TRAN-PROC-TS from its 26-byte text; the parsed value is null if the text is in neither layout. */
    public void setProcTsText(String text) {
        this.procTsText = requireTimestampText(text);
        this.procTs = TransactionTimestamp.parse(text).orElse(null);
    }

    private static String requireTimestampText(String text) {
        if (text == null || text.length() != TransactionTimestamp.LENGTH) {
            throw new IllegalArgumentException("timestamp text must be " + TransactionTimestamp.LENGTH + " characters");
        }
        return text;
    }
}
