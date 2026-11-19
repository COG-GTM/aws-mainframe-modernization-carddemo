package com.carddemo.reference.domain;

import java.io.Serializable;

/** TRAN-CAT-KEY: TRANCAT-ACCT-ID 9(11), TRANCAT-TYPE-CD X(02), TRANCAT-CD 9(04). */
public record TranCatBalanceId(Long acctId, String typeCd, Long catCd) implements Serializable {

    /** For JPA {@code @IdClass} only. */
    public TranCatBalanceId() {
        this(null, null, null);
    }
}
