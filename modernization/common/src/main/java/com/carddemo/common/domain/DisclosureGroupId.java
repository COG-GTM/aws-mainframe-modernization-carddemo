package com.carddemo.common.domain;

import java.io.Serializable;
import java.util.Objects;

public class DisclosureGroupId implements Serializable {

    private String acctGroupId;
    private String typeCd;
    private Integer catCd;

    public DisclosureGroupId() {
    }

    public DisclosureGroupId(String acctGroupId, String typeCd, Integer catCd) {
        this.acctGroupId = acctGroupId;
        this.typeCd = typeCd;
        this.catCd = catCd;
    }

    public String getAcctGroupId() {
        return acctGroupId;
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
        if (!(o instanceof DisclosureGroupId that)) {
            return false;
        }
        return Objects.equals(acctGroupId, that.acctGroupId) && Objects.equals(typeCd, that.typeCd) && Objects.equals(catCd, that.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(acctGroupId, typeCd, catCd);
    }
}
