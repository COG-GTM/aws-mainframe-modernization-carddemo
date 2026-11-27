package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

import java.io.Serializable;
import java.util.Objects;

/** TRAN-CAT-KEY group of CVTRA01Y (VSAM KSDS TCATBALF key, 17 bytes). */
@Embeddable
public class TransactionCategoryBalanceId implements Serializable {

    /** TRANCAT-ACCT-ID PIC 9(11) */
    @Column(name = "acct_id")
    private Long accountId;

    /** TRANCAT-TYPE-CD PIC X(02) */
    @Column(name = "type_cd", length = 2)
    private String typeCode;

    /** TRANCAT-CD PIC 9(04) */
    @Column(name = "cat_cd")
    private Integer categoryCode;

    public TransactionCategoryBalanceId() {
    }

    public TransactionCategoryBalanceId(Long accountId, String typeCode, Integer categoryCode) {
        this.accountId = accountId;
        this.typeCode = typeCode;
        this.categoryCode = categoryCode;
    }

    public Long getAccountId() {
        return accountId;
    }

    public void setAccountId(Long accountId) {
        this.accountId = accountId;
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
        return this == o || (o instanceof TransactionCategoryBalanceId that && Objects.equals(accountId, that.accountId) && Objects.equals(typeCode, that.typeCode) && Objects.equals(categoryCode, that.categoryCode));
    }

    @Override
    public int hashCode() {
        return Objects.hash(accountId, typeCode, categoryCode);
    }

    @Override
    public String toString() {
        return "TransactionCategoryBalanceId[" + accountId + "," + typeCode + "," + categoryCode + "]";
    }
}
