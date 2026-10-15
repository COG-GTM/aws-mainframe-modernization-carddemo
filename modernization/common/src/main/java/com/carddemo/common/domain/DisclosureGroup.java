package com.carddemo.common.domain;

import jakarta.persistence.*;
import java.math.BigDecimal;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Table disclosure_group; legacy source DISCGRP / CVTRA02Y. */
@Entity
@Table(name = "disclosure_group")
@IdClass(DisclosureGroupId.class)
public class DisclosureGroup {

    @Id
    @Column(name = "acct_group_id")
    private String acctGroupId;

    @Id
    @JdbcTypeCode(SqlTypes.CHAR)
    @Column(name = "type_cd")
    private String typeCd;

    @Id
    @Column(name = "cat_cd")
    private Integer catCd;

    @Column(name = "int_rate")
    private BigDecimal intRate;

    public String getAcctGroupId() {
        return acctGroupId;
    }

    public void setAcctGroupId(String acctGroupId) {
        this.acctGroupId = acctGroupId;
    }

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

    public BigDecimal getIntRate() {
        return intRate;
    }

    public void setIntRate(BigDecimal intRate) {
        this.intRate = intRate;
    }
}
