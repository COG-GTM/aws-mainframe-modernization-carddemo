package com.carddemo.common.domain;

import java.io.Serializable;
import java.util.Objects;

public class TranCatBalanceId implements Serializable {

    private Long acctId;
    private String typeCd;
    private Integer catCd;

    public TranCatBalanceId() {
    }

    public TranCatBalanceId(Long acctId, String typeCd, Integer catCd) {
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
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof TranCatBalanceId that)) {
            return false;
        }
        return Objects.equals(acctId, that.acctId) && Objects.equals(typeCd, that.typeCd) && Objects.equals(catCd, that.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(acctId, typeCd, catCd);
    }
}
