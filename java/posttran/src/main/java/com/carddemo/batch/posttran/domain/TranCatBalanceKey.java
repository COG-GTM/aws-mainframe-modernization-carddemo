package com.carddemo.batch.posttran.domain;

import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.ZonedDecimal;
import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import java.io.Serializable;

/** TRAN-CAT-KEY: TRANCAT-ACCT-ID 9(11) + TRANCAT-TYPE-CD X(02) + TRANCAT-CD 9(04). */
@Embeddable
public record TranCatBalanceKey(
        @Column(name = "acct_id") long acctId,
        @Column(name = "type_cd", length = 2) String typeCd,
        @Column(name = "cat_cd") int catCd) implements Serializable {

    /** The key as the 17-byte FD-TRAN-CAT-KEY. */
    public String asRecordKey() {
        return ZonedDecimal.encodeUnsigned(acctId, 11) + Pic.x(typeCd, 2) + ZonedDecimal.encodeUnsigned(catCd, 4);
    }
}
