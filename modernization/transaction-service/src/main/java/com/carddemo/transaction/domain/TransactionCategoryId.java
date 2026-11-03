package com.carddemo.transaction.domain;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;
import java.util.Objects;

/** TRAN-TYPE-CD + TRAN-CAT-CD, the key of TRANCATG (copybook CVTRA04Y). */
@Embeddable
public class TransactionCategoryId implements Serializable {

    @Column(name = "type_cd", length = 2, nullable = false)
    private String typeCd;

    @Column(name = "cat_cd", nullable = false)
    private Integer catCd;

    protected TransactionCategoryId() {
    }

    public TransactionCategoryId(String typeCd, Integer catCd) {
        this.typeCd = typeCd;
        this.catCd = catCd;
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
        if (!(other instanceof TransactionCategoryId that)) {
            return false;
        }
        return Objects.equals(typeCd, that.typeCd) && Objects.equals(catCd, that.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(typeCd, catCd);
    }
}
