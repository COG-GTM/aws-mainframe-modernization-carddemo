package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.io.Serializable;
import java.util.Objects;

/** TRAN-CAT-KEY group of CVTRA04Y (VSAM KSDS TRANCATG key, 6 bytes). */
@Embeddable
public class TransactionCategoryId implements Serializable {

    /** TRAN-TYPE-CD PIC X(02) */
    @Column(name = "type_cd", length = 2)
    private String typeCode;

    /** TRAN-CAT-CD PIC 9(04) */
    @Column(name = "cat_cd")
    private Integer categoryCode;

    public TransactionCategoryId() {
    }

    public TransactionCategoryId(String typeCode, Integer categoryCode) {
        this.typeCode = typeCode;
        this.categoryCode = categoryCode;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public Integer getCategoryCode() {
        return categoryCode;
    }

    public void setCategoryCode(Integer categoryCode) {
        this.categoryCode = categoryCode;
    }

    @Override
    public boolean equals(Object o) {
        return this == o || (o instanceof TransactionCategoryId that && Objects.equals(typeCode, that.typeCode) && Objects.equals(categoryCode, that.categoryCode));
    }

    @Override
    public int hashCode() {
        return Objects.hash(typeCode, categoryCode);
    }

    @Override
    public String toString() {
        return "TransactionCategoryId[" + typeCode + "," + categoryCode + "]";
    }
}
