package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/** Primary key of {@link TransactionCategory}, in VSAM key order. */
@Embeddable
public record TransactionCategoryId(
        @Column(name = "type_cd", nullable = false, length = 2) String typeCd,
        @Column(name = "cat_cd", nullable = false) Short catCd) {
}
