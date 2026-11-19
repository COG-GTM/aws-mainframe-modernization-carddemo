package com.carddemo.transaction.domain;

import com.carddemo.common.record.FixedRecord;

import static com.carddemo.common.record.Copybooks.CVTRA05Y;

/** {@link Transaction} ↔ CVTRA05Y record. */
public final class TransactionRecords {

    private TransactionRecords() {
    }

    public static Transaction fromRecord(String text) {
        FixedRecord r = CVTRA05Y.parse(text);
        Transaction t = new Transaction();
        t.setTranId(r.text("TRAN-ID"));
        t.setTypeCd(r.text("TRAN-TYPE-CD"));
        t.setCatCd((int) r.number("TRAN-CAT-CD"));
        t.setSource(r.text("TRAN-SOURCE"));
        t.setDescription(r.text("TRAN-DESC"));
        t.setAmount(r.decimal("TRAN-AMT"));
        t.setAmountNegativeZero(r.negativeZero("TRAN-AMT"));
        t.setMerchantId(r.number("TRAN-MERCHANT-ID"));
        t.setMerchantName(r.text("TRAN-MERCHANT-NAME"));
        t.setMerchantCity(r.text("TRAN-MERCHANT-CITY"));
        t.setMerchantZip(r.text("TRAN-MERCHANT-ZIP"));
        t.setCardNum(r.text("TRAN-CARD-NUM"));
        t.setOrigTs(r.text("TRAN-ORIG-TS"));
        t.setProcTs(r.text("TRAN-PROC-TS"));
        return t;
    }

    public static String toRecord(Transaction t) {
        return CVTRA05Y.blank()
                .set("TRAN-ID", t.getTranId())
                .set("TRAN-TYPE-CD", t.getTypeCd())
                .set("TRAN-CAT-CD", t.getCatCd())
                .set("TRAN-SOURCE", t.getSource())
                .set("TRAN-DESC", t.getDescription())
                .set("TRAN-AMT", t.getAmount(), t.isAmountNegativeZero())
                .set("TRAN-MERCHANT-ID", t.getMerchantId())
                .set("TRAN-MERCHANT-NAME", t.getMerchantName())
                .set("TRAN-MERCHANT-CITY", t.getMerchantCity())
                .set("TRAN-MERCHANT-ZIP", t.getMerchantZip())
                .set("TRAN-CARD-NUM", t.getCardNum())
                .set("TRAN-ORIG-TS", t.getOrigTs())
                .set("TRAN-PROC-TS", t.getProcTs())
                .toString();
    }
}
