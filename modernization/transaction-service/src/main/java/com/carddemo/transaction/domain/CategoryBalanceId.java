package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;
import java.util.Objects;

/** TRANCAT-ACCT-ID + TRANCAT-TYPE-CD + TRANCAT-CD, the key of TCATBAL (copybook CVTRA01Y). */
@Embeddable
public class CategoryBalanceId implements Serializable {

    @Column(name = "acct_id", nullable = false)
    private Long acctId;

    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private Integer catCd;

    protected CategoryBalanceId() {
    }

    public CategoryBalanceId(Long acctId, String typeCd, Integer catCd) {
        this.acctId = acctId;
        this.typeCd = typeCd;
        this.catCd = catCd;
    }

    public Long getAcctId() {
        return acctId;
    }

    public String getTypeCd() {
        return typeCd;
    }

    public Integer getCatCd() {
        return catCd;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof CategoryBalanceId that)) {
            return false;
        }
        return Objects.equals(acctId, that.acctId)
                && Objects.equals(typeCd, that.typeCd)
                && Objects.equals(catCd, that.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(acctId, typeCd, catCd);
    }
}
