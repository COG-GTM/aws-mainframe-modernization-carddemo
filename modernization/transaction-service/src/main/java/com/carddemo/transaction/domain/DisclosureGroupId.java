package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;
import java.util.Objects;

/** DIS-ACCT-GROUP-ID + DIS-TRAN-TYPE-CD + DIS-TRAN-CAT-CD, the key of DISCGRP (copybook CVTRA02Y). */
@Embeddable
public class DisclosureGroupId implements Serializable {

    @Column(name = "acct_group_id", length = 10, nullable = false)
    private String acctGroupId;

    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private Integer catCd;

    protected DisclosureGroupId() {
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
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof DisclosureGroupId that)) {
            return false;
        }
        return Objects.equals(acctGroupId, that.acctGroupId)
                && Objects.equals(typeCd, that.typeCd)
                && Objects.equals(catCd, that.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(acctGroupId, typeCd, catCd);
    }
}
