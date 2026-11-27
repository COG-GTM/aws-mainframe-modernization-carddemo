package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;

/** Fixed-width TRAN-CAT-BAL-RECORD. Layout: copybook CVTRA01Y. */
public record TranCatBalanceRecord(
        @CobolField("TRAN-CAT-KEY") Key id,
        @CobolField("TRAN-CAT-BAL") BigDecimal balance) {

    /** TRAN-CAT-KEY. */
    public record Key(
            @CobolField("TRANCAT-ACCT-ID") Long accountId,
            @CobolField("TRANCAT-TYPE-CD") String typeCode,
            @CobolField("TRANCAT-CD") Integer categoryCode) {
    }
}
