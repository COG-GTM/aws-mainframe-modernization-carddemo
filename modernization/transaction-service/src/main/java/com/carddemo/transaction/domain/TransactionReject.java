package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.Instant;

/**
 * DALYREJS record written by CBTRN02C paragraph 2500-WRITE-REJECT-REC: the rejected daily
 * transaction plus WS-VALIDATION-FAIL-REASON and its description.
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

    @Column(name = "card_num", length = 16)
    private String cardNum;

    @Column(name = "reason_code", nullable = false)
    private int reasonCode;

    @Column(name = "reason_desc", length = 80)
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

    public Long getRejectId() {
        return rejectId;
    }

    public String getTranId() {
        return tranId;
    }

    public String getCardNum() {
        return cardNum;
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
