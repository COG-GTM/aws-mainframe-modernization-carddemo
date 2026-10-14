package com.carddemo.batch.posttran.domain;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/** XREFFILE KSDS (CARDXREF.VSAM.KSDS), CARD-XREF-RECORD from CVACT03Y, key XREF-CARD-NUM X(16). */
@Entity
@Table(name = "card_xref")
public class CardXref {

    public static final int LENGTH = 50;

    @Id
    @Column(name = "card_num", length = 16)
    private String cardNum;
    @Column(name = "cust_id", nullable = false)
    private long custId;
    @Column(name = "acct_id", nullable = false)
    private long acctId;
    @Column(name = "filler", length = 14, nullable = false)
    private String filler = Pic.spaces(14);

    protected CardXref() {
    }

    public CardXref(String cardNum, long custId, long acctId) {
        this.cardNum = Pic.x(cardNum, 16);
        this.custId = custId;
        this.acctId = acctId;
    }

    public static CardXref fromRecord(String r) {
        CardXref x = new CardXref(r.substring(0, 16), ZonedDecimal.decodeUnsigned(r.substring(16, 25)),
                ZonedDecimal.decodeUnsigned(r.substring(25, 36)));
        x.filler = r.substring(36, 50);
        return x;
    }

    public String toRecord() {
        return Pic.x(cardNum, 16) + ZonedDecimal.encodeUnsigned(custId, 9)
                + ZonedDecimal.encodeUnsigned(acctId, 11) + Pic.x(filler, 14);
    }

    public String getCardNum() { return cardNum; }
    public long getCustId() { return custId; }
    public long getAcctId() { return acctId; }
}
