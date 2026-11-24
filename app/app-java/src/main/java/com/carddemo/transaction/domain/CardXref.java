package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Table;

/**
 * CARD-XREF-RECORD (copybook CVACT03Y, RECLN 50) - CARDXREF VSAM KSDS keyed by XREF-CARD-NUM, with the
 * CXACAIX alternate index on XREF-ACCT-ID.
 */
@Entity
@Table(name = "CARDXREF", indexes = @Index(name = "CXACAIX", columnList = "XREF_ACCT_ID"))
public class CardXref {

    @Id
    @Column(name = "XREF_CARD_NUM", length = 16)
    private String cardNum;                 // PIC X(16)
    @Column(name = "XREF_CUST_ID")
    private Long custId;                    // PIC 9(09)
    @Column(name = "XREF_ACCT_ID")
    private Long acctId;                    // PIC 9(11)

    public String getCardNum() { return cardNum; }
    public void setCardNum(String v) { this.cardNum = Pic.x(v, 16); }
    public Long getCustId() { return custId; }
    public void setCustId(long v) { this.custId = Pic.n9(v, 9); }
    public Long getAcctId() { return acctId; }
    public void setAcctId(long v) { this.acctId = Pic.n9(v, 11); }
}
