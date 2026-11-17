package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/** Primary key of {@link DisclosureGroup}, in VSAM key order. */
@Embeddable
public record DisclosureGroupId(
        @Column(name = "acct_group_id", nullable = false, length = 10) String acctGroupId,
        @Column(name = "tran_type_cd", nullable = false, length = 2) String tranTypeCd,
        @Column(name = "tran_cat_cd", nullable = false) Short tranCatCd) {
}
