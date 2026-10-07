package com.carddemo.transaction;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;

/**
 * Composite primary key of {@code transaction_category}, in VSAM key order.
 *
 * @param tranTypeCd TRAN-TYPE-CD
 * @param tranCatCd TRAN-CAT-CD
 */
@Embeddable
public record TransactionCategoryId(
        @Column(name = "tran_type_cd") String tranTypeCd,
        @Column(name = "tran_cat_cd") int tranCatCd) implements Serializable {
}
