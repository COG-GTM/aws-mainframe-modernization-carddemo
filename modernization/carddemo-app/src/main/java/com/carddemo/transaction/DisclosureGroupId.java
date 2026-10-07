package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;

/**
 * Composite primary key of {@code disclosure_group}, in VSAM key order.
 *
 * @param acctGroupId DIS-ACCT-GROUP-ID
 * @param tranTypeCd DIS-TRAN-TYPE-CD
 * @param tranCatCd DIS-TRAN-CAT-CD
 */
@Embeddable
public record DisclosureGroupId(
        @Column(name = "acct_group_id") String acctGroupId,
        @Column(name = "tran_type_cd") String tranTypeCd,
        @Column(name = "tran_cat_cd") int tranCatCd) implements Serializable {
}
