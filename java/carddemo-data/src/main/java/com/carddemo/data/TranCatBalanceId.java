package com.carddemo.data;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;

/** Primary key of {@link TranCatBalance}. */
@Embeddable
public record TranCatBalanceId(
        @Column(name = "acct_id", nullable = false) Long acctId,
        @Column(name = "type_cd", nullable = false, length = 2) String typeCd,
        @Column(name = "cat_cd", nullable = false) Integer catCd) implements Serializable {
}
