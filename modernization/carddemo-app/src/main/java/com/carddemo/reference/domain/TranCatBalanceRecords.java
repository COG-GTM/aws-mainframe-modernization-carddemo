package com.carddemo.reference.domain;

import com.carddemo.common.record.FixedRecord;

import static com.carddemo.common.record.Copybooks.CVTRA01Y;

/** {@link TranCatBalance} ↔ CVTRA01Y record. */
public final class TranCatBalanceRecords {

    private TranCatBalanceRecords() {
    }

    public static TranCatBalance fromRecord(String text) {
        FixedRecord r = CVTRA01Y.parse(text);
        TranCatBalance b = new TranCatBalance();
        b.setAcctId(r.number("TRANCAT-ACCT-ID"));
        b.setTypeCd(r.text("TRANCAT-TYPE-CD"));
        b.setCatCd((int) r.number("TRANCAT-CD"));
        b.setBalance(r.decimal("TRAN-CAT-BAL"));
        b.setBalanceNegativeZero(r.negativeZero("TRAN-CAT-BAL"));
        return b;
    }

    public static String toRecord(TranCatBalance b) {
        return CVTRA01Y.blank()
                .set("TRANCAT-ACCT-ID", b.getAcctId())
                .set("TRANCAT-TYPE-CD", b.getTypeCd())
                .set("TRANCAT-CD", b.getCatCd())
                .set("TRAN-CAT-BAL", b.getBalance(), b.isBalanceNegativeZero())
                .toString();
    }
}
