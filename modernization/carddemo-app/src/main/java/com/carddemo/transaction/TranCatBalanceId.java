package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;

/**
 * Composite primary key of {@code tran_cat_balance}, in VSAM key order.
 *
 * @param acctId TRANCAT-ACCT-ID
 * @param tranTypeCd TRANCAT-TYPE-CD
 * @param tranCatCd TRANCAT-CD
 */
@Embeddable
public record TranCatBalanceId(
        @Column(name = "acct_id") long acctId,
        @Column(name = "tran_type_cd") String tranTypeCd,
        @Column(name = "tran_cat_cd") int tranCatCd) implements Serializable {
}
