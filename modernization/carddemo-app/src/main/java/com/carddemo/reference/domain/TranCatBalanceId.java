package com.carddemo.reference.domain;

import java.io.Serializable;
import java.util.Objects;

/** TRAN-CAT-KEY: TRANCAT-ACCT-ID 9(11) + TRANCAT-TYPE-CD X(02) + TRANCAT-CD 9(04). */
public class TranCatBalanceId implements Serializable {

    private long acctId;
    private String typeCd;
    private int catCd;

    public TranCatBalanceId() {
    }

    public TranCatBalanceId(long acctId, String typeCd, int catCd) {
        this.acctId = acctId;
        this.typeCd = typeCd;
        this.catCd = catCd;
    }

    public long getAcctId() {
        return acctId;
    }

    public String getTypeCd() {
        return typeCd;
    }

    public int getCatCd() {
        return catCd;
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof TranCatBalanceId other
                && acctId == other.acctId && catCd == other.catCd && Objects.equals(typeCd, other.typeCd);
    }

    @Override
    public int hashCode() {
        return Objects.hash(acctId, typeCd, catCd);
    }
}
