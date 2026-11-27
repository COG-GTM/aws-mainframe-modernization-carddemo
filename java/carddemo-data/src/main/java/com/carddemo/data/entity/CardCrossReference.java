package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Table;

/**
 * Card / customer / account cross reference (VSAM KSDS CCXREF, RECLN 50).
 * <p>Source copybook: CVACT03Y.
 */
@Entity
@Table(name = "card_xref", indexes = {
        @Index(name = "ix_card_xref_acct_id", columnList = "acct_id")
})
public class CardCrossReference {

    /** XREF-CARD-NUM PIC X(16) - VSAM KSDS key (CCXREF) */
    @Id
    @Column(name = "card_num", length = 16)
    private String cardNumber;

    /** XREF-CUST-ID PIC 9(09) */
    @Column(name = "cust_id", nullable = false)
    private Long customerId;

    /** XREF-ACCT-ID PIC 9(11) - alternate index CXACAIX */
    @Column(name = "acct_id", nullable = false)
    private Long accountId;

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public Long getCustomerId() {
        return customerId;
    }

    public void setCustomerId(Long customerId) {
        this.customerId = customerId;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
    }
}
