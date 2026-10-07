package com.carddemo.card;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.Immutable;

/**
 * JPA entity for table {@code card_xref}. Card cross-reference record (CARDXREF KSDS, key XREF-CARD-NUM; CXACAIX on
 * XREF-ACCT-ID); read by COACTVWC, COACTUPC, COBIL00C, COTRN02C, CBTRN01C/02C, CBACT04C and CBSTM03A. Field
 * Javadocs carry the CVACT03Y item names; {@link CardXrefRecord} is the fixed-width value.
 */
@Entity
@Immutable
@Table(name = "card_xref")
public class CardXref {

    /** XREF-CARD-NUM PIC X(16). */
    @Id
    @Column(name = "card_num")
    private String cardNum;

    /** XREF-CUST-ID PIC 9(09). */
    @Column(name = "cust_id")
    private int custId;

    /** XREF-ACCT-ID PIC 9(11). */
    @Column(name = "acct_id")
    private long acctId;

    protected CardXref() {
    }

    public static CardXref from(CardXrefRecord record) {
        CardXref entity = new CardXref();
        entity.cardNum = record.cardNum();
        entity.custId = record.custId();
        entity.acctId = record.acctId();
        return entity;
    }

    public CardXrefRecord toRecord() {
        return new CardXrefRecord(cardNum, custId, acctId);
    }

    public String getCardNum() {
        return cardNum;
    }

    public int getCustId() {
        return custId;
    }

    public long getAcctId() {
        return acctId;
    }
}
