package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDateTime;

/**
 * DALYREJS record written by CBTRN02C paragraph 2500-WRITE-REJECT-REC: the whole rejected
 * DALYTRAN-RECORD (REJECT-TRAN-DATA) plus WS-VALIDATION-FAIL-REASON and its description.
 */
@Entity
@Table(name = "transaction_rejects")
public class TransactionReject {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "reject_id")
    private Long rejectId;

    @Column(name = "tran_id", length = 16, nullable = false)
    private String tranId;

    @Column(name = "type_cd", length = 2)
    private String typeCd;

    @Column(name = "cat_cd")
    private Integer catCd;

    @Column(name = "source", length = 10)
    private String source;

    @Column(name = "description", length = 100)
    private String description;

    @Column(name = "amount", precision = 11, scale = 2)
    private BigDecimal amount;

    @Column(name = "merchant_id")
    private Long merchantId;

    @Column(name = "merchant_name", length = 50)
    private String merchantName;

    @Column(name = "merchant_city", length = 50)
    private String merchantCity;

    @Column(name = "merchant_zip", length = 10)
    private String merchantZip;

    @Column(name = "card_num", length = 16)
    private String cardNum;

    @Column(name = "orig_ts")
    private LocalDateTime origTs;

    @Column(name = "proc_ts")
    private LocalDateTime procTs;

    @Column(name = "reason_code", nullable = false)
    private int reasonCode;

    @Column(name = "reason_desc", length = 76)
    private String reasonDesc;

    @Column(name = "rejected_at", nullable = false)
    private Instant rejectedAt = Instant.now();

    protected TransactionReject() {
    }

    public TransactionReject(String tranId, String cardNum, int reasonCode, String reasonDesc) {
        this.tranId = tranId;
        this.cardNum = cardNum;
        this.reasonCode = reasonCode;
        this.reasonDesc = reasonDesc;
    }

    /** MOVE DALYTRAN-RECORD TO REJECT-TRAN-DATA, MOVE WS-VALIDATION-TRAILER TO VALIDATION-TRAILER. */
    public static TransactionReject of(DailyTransaction daily, int reasonCode, String reasonDesc) {
        TransactionReject reject = new TransactionReject(daily.getTranId(), daily.getCardNum(),
                reasonCode, reasonDesc);
        reject.typeCd = daily.getTypeCd();
        reject.catCd = daily.getCatCd();
        reject.source = daily.getSource();
        reject.description = daily.getDescription();
        reject.amount = daily.getAmount();
        reject.merchantId = daily.getMerchantId();
        reject.merchantName = daily.getMerchantName();
        reject.merchantCity = daily.getMerchantCity();
        reject.merchantZip = daily.getMerchantZip();
        reject.origTs = daily.getOrigTs();
        reject.procTs = daily.getProcTs();
        return reject;
    }

    public Long getRejectId() {
        return rejectId;
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

    public String getDescription() {
        return description;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public Long getMerchantId() {
        return merchantId;
    }

    public String getMerchantName() {
        return merchantName;
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public String getMerchantZip() {
        return merchantZip;
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

    public int getReasonCode() {
        return reasonCode;
    }

    public String getReasonDesc() {
        return reasonDesc;
    }

    public Instant getRejectedAt() {
        return rejectedAt;
    }
}
