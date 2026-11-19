package com.carddemo.transaction.batch;

import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.transaction.domain.Transaction;

/** CVTRA05Y TRAN-RECORD (TRANSACT, 350 bytes) and its mapping to {@link Transaction}. FILLER is spaces. */
public final class TransactionRecordLayout {

    public static final int LENGTH = 350;

    public static final RecordLayout LAYOUT = RecordLayout.builder("CVTRA05Y", LENGTH)
            .alphanumeric("TRAN-ID", 16)
            .alphanumeric("TRAN-TYPE-CD", 2)
            .zoned("TRAN-CAT-CD", 4, 0, false)
            .alphanumeric("TRAN-SOURCE", 10)
            .alphanumeric("TRAN-DESC", 100)
            .zoned("TRAN-AMT", 9, 2, true)
            .zoned("TRAN-MERCHANT-ID", 9, 0, false)
            .alphanumeric("TRAN-MERCHANT-NAME", 50)
            .alphanumeric("TRAN-MERCHANT-CITY", 50)
            .alphanumeric("TRAN-MERCHANT-ZIP", 10)
            .alphanumeric("TRAN-CARD-NUM", 16)
            .alphanumeric("TRAN-ORIG-TS", 26)
            .alphanumeric("TRAN-PROC-TS", 26)
            .filler(20)
            .build();

    private TransactionRecordLayout() {
    }

    public static Transaction toEntity(CobolRecord r) {
        Transaction t = new Transaction(text(r, "TRAN-ID"));
        t.setTypeCd(text(r, "TRAN-TYPE-CD"));
        t.setCatCd(r.getLong("TRAN-CAT-CD"));
        t.setSource(text(r, "TRAN-SOURCE"));
        t.setDescription(text(r, "TRAN-DESC"));
        t.setAmount(r.getDecimal("TRAN-AMT"));
        t.setMerchantId(r.getLong("TRAN-MERCHANT-ID"));
        t.setMerchantName(text(r, "TRAN-MERCHANT-NAME"));
        t.setMerchantCity(text(r, "TRAN-MERCHANT-CITY"));
        t.setMerchantZip(text(r, "TRAN-MERCHANT-ZIP"));
        t.setCardNum(text(r, "TRAN-CARD-NUM"));
        t.setOrigTsText(r.getString("TRAN-ORIG-TS"));
        t.setProcTsText(r.getString("TRAN-PROC-TS"));
        return t;
    }

    public static CobolRecord toRecord(Transaction t, CodePage codePage) {
        return LAYOUT.blank(codePage)
                .setString("TRAN-ID", t.getTranId())
                .setString("TRAN-TYPE-CD", t.getTypeCd())
                .setLong("TRAN-CAT-CD", t.getCatCd())
                .setString("TRAN-SOURCE", t.getSource())
                .setString("TRAN-DESC", t.getDescription())
                .setDecimal("TRAN-AMT", t.getAmount())
                .setLong("TRAN-MERCHANT-ID", t.getMerchantId())
                .setString("TRAN-MERCHANT-NAME", t.getMerchantName())
                .setString("TRAN-MERCHANT-CITY", t.getMerchantCity())
                .setString("TRAN-MERCHANT-ZIP", t.getMerchantZip())
                .setString("TRAN-CARD-NUM", t.getCardNum())
                .setString("TRAN-ORIG-TS", t.getOrigTsText())
                .setString("TRAN-PROC-TS", t.getProcTsText());
    }

    private static String text(CobolRecord r, String field) {
        return r.getString(field).stripTrailing();
    }
}
