package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.io.Serializable;
import java.util.Objects;

/**
 * TRAN-CAT-KEY / FD-TRAN-CAT-KEY: TRANCAT-ACCT-ID 9(11) + TRANCAT-TYPE-CD X(02) + TRANCAT-CD 9(04).
 */
@Embeddable
public class TransactionCategoryBalanceId implements Serializable {

    @Column(name = "TRANCAT_ACCT_ID")
    private Long acctId;
    @Column(name = "TRANCAT_TYPE_CD", length = 2)
    private String typeCd;
    @Column(name = "TRANCAT_CD")
    private Integer catCd;

    protected TransactionCategoryBalanceId() {
    }

    public TransactionCategoryBalanceId(long acctId, String typeCd, int catCd) {
        this.acctId = Pic.n9(acctId, 11);
        this.typeCd = Pic.x(typeCd, 2);
        this.catCd = Pic.n9(catCd, 4);
    }

    public Long getAcctId() { return acctId; }
    public String getTypeCd() { return typeCd; }
    public Integer getCatCd() { return catCd; }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof TransactionCategoryBalanceId other)) {
            return false;
        }
        return Objects.equals(acctId, other.acctId) && Objects.equals(typeCd, other.typeCd)
                && Objects.equals(catCd, other.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(acctId, typeCd, catCd);
    }

    @Override
    public String toString() {
        return Pic.zeroFill(acctId, 11) + typeCd + Pic.zeroFill(catCd, 4);
    }
}
