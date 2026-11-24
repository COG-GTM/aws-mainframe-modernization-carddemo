package com.carddemo.common.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table transaction_category; legacy source TRANCATG / CVTRA04Y. */
@Entity
@Table(name = "transaction_category")
@IdClass(TransactionCategoryId.class)
public class TransactionCategory {

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "type_cd")
    private String typeCd;

    @Id
    @Column(name = "cat_cd")
    private Integer catCd;

    @Column(name = "description")
    private String description;

    public String getTypeCd() {
        return typeCd;
    }

    public void setTypeCd(String typeCd) {
        this.typeCd = typeCd;
    }

    public Integer getCatCd() {
        return catCd;
    }

    public void setCatCd(Integer catCd) {
        this.catCd = catCd;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
