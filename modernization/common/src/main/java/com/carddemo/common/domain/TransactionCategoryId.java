package com.carddemo.common.domain;

import java.io.Serializable;
import java.util.Objects;

public class TransactionCategoryId implements Serializable {

    private String typeCd;
    private Integer catCd;

    public TransactionCategoryId() {
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
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof TransactionCategoryId that)) {
            return false;
        }
        return Objects.equals(typeCd, that.typeCd) && Objects.equals(catCd, that.catCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(typeCd, catCd);
    }
}
