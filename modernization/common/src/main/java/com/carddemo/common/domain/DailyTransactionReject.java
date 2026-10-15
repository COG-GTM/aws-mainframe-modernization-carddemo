package com.carddemo.common.domain;

import jakarta.persistence.*;
import java.math.BigDecimal;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;
import java.time.LocalDateTime;

/** Table daily_transaction_reject; legacy DALYREJS (350-byte transaction + 80-byte validation trailer). */
@Entity
@Table(name = "daily_transaction_reject")
public class DailyTransactionReject {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "tran_id")
    private String tranId;

    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "card_num")
    private String cardNum;

    @Column(name = "amount")
    private BigDecimal amount;

    @Column(name = "reason_code")
    private int reasonCode;

    @Column(name = "reason_desc")
    private String reasonDesc;

    @Column(name = "batch_id")
    private String batchId;

    @Column(name = "rejected_at")
    private LocalDateTime rejectedAt;

    protected DailyTransactionReject() {
    }

    public DailyTransactionReject(String tranId, String cardNum, BigDecimal amount, int reasonCode,
                                  String reasonDesc, String batchId, LocalDateTime rejectedAt) {
        this.tranId = tranId;
        this.cardNum = cardNum;
        this.amount = amount;
        this.reasonCode = reasonCode;
        this.reasonDesc = reasonDesc;
        this.batchId = batchId;
        this.rejectedAt = rejectedAt;
    }

    public Long getId() {
        return id;
    }

    public String getTranId() {
        return tranId;
    }

    public String getCardNum() {
        return cardNum;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public int getReasonCode() {
        return reasonCode;
    }

    public String getReasonDesc() {
        return reasonDesc;
    }

    public String getBatchId() {
        return batchId;
    }

    public LocalDateTime getRejectedAt() {
        return rejectedAt;
    }
}
