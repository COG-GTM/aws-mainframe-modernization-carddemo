package com.carddemo.common.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table card_xref; legacy source CCXREF / CVACT03Y (AIX CXACAIX on acct_id). */
@Entity
@Table(name = "card_xref")
public class CardXref {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "card_num")
    private String cardNum;

    @Column(name = "cust_id")
    private Long custId;

    @Column(name = "acct_id")
    private Long acctId;

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String cardNum) {
        this.cardNum = cardNum;
    }

    public Long getCustId() {
        return custId;
    }

    public void setCustId(Long custId) {
        this.custId = custId;
    }

    public Long getAcctId() {
        return acctId;
    }

    public void setAcctId(Long acctId) {
        this.acctId = acctId;
    }
}
