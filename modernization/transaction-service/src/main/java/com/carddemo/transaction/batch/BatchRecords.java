package com.carddemo.transaction.batch;

import com.carddemo.common.cobol.CobolValues;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.DalyrejsRecordLayout;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.common.record.layout.TranCatBalRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.DailyTransaction;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionReject;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Converts between the transaction-service tables and the fixed-width records of the batch DDs
 * (DALYTRAN, DALYREJS, TRANSACT/SYSTRAN, TCATBALF). Timestamps the program copies from DALYTRAN keep
 * the file layout ({@code YYYY-MM-DD HH:MM:SS.ffffff}); timestamps the program stamps itself use
 * the DB2 layout of {@code Z-GET-DB2-FORMAT-TIMESTAMP} ({@code YYYY-MM-DD-HH.MM.SS.ffffff}).
 */
final class BatchRecords {

    /** TRAN-SOURCE of the transactions INTCALC writes. */
    static final String SYSTEM_SOURCE = "System";

    static final DateTimeFormatter FILE_TIMESTAMP = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private BatchRecords() {
    }

    static DailyTransaction toDailyTransaction(CobolRecord r) {
        DailyTransaction daily = new DailyTransaction(r.getTrimmed(DalytranRecordLayout.DALYTRAN_ID),
                r.getTrimmed(DalytranRecordLayout.DALYTRAN_CARD_NUM),
                r.getTrimmed(DalytranRecordLayout.DALYTRAN_TYPE_CD),
                r.getInt(DalytranRecordLayout.DALYTRAN_CAT_CD),
                r.get(DalytranRecordLayout.DALYTRAN_AMT),
                CobolValues.timestamp(r.get(DalytranRecordLayout.DALYTRAN_ORIG_TS)));
        daily.setSource(CobolValues.trim(r.get(DalytranRecordLayout.DALYTRAN_SOURCE)));
        daily.setDescription(CobolValues.trim(r.get(DalytranRecordLayout.DALYTRAN_DESC)));
        daily.setMerchantId(r.getLong(DalytranRecordLayout.DALYTRAN_MERCHANT_ID));
        daily.setMerchantName(CobolValues.trim(r.get(DalytranRecordLayout.DALYTRAN_MERCHANT_NAME)));
        daily.setMerchantCity(CobolValues.trim(r.get(DalytranRecordLayout.DALYTRAN_MERCHANT_CITY)));
        daily.setMerchantZip(CobolValues.trim(r.get(DalytranRecordLayout.DALYTRAN_MERCHANT_ZIP)));
        daily.setProcTs(CobolValues.timestamp(r.get(DalytranRecordLayout.DALYTRAN_PROC_TS)));
        return daily;
    }

    /**
     * DALYREJS: the reject's DALYTRAN record plus reason code and description. {@code original} is the
     * record as read from the DALYTRAN file (kept byte for byte, filler included, as CBTRN02C copies it);
     * without it the record is rebuilt from the reject row.
     */
    static CobolRecord toRejectRecord(TransactionReject reject, CodePage codePage, CobolRecord original) {
        if (original != null) {
            return DalyrejsRecordLayout.of(original, reject.getReasonCode(), reject.getReasonDesc());
        }
        CobolRecord daily = CobolRecord.initialized(DalytranRecordLayout.LAYOUT, codePage)
                .set(DalytranRecordLayout.DALYTRAN_ID, reject.getTranId())
                .set(DalytranRecordLayout.DALYTRAN_TYPE_CD, reject.getTypeCd())
                .set(DalytranRecordLayout.DALYTRAN_CAT_CD, number(reject.getCatCd()))
                .set(DalytranRecordLayout.DALYTRAN_SOURCE, reject.getSource())
                .set(DalytranRecordLayout.DALYTRAN_DESC, reject.getDescription())
                .set(DalytranRecordLayout.DALYTRAN_AMT, amount(reject.getAmount()))
                .set(DalytranRecordLayout.DALYTRAN_MERCHANT_ID, number(reject.getMerchantId()))
                .set(DalytranRecordLayout.DALYTRAN_MERCHANT_NAME, reject.getMerchantName())
                .set(DalytranRecordLayout.DALYTRAN_MERCHANT_CITY, reject.getMerchantCity())
                .set(DalytranRecordLayout.DALYTRAN_MERCHANT_ZIP, reject.getMerchantZip())
                .set(DalytranRecordLayout.DALYTRAN_CARD_NUM, reject.getCardNum())
                .set(DalytranRecordLayout.DALYTRAN_ORIG_TS, fileTimestamp(reject.getOrigTs()))
                .set(DalytranRecordLayout.DALYTRAN_PROC_TS, fileTimestamp(reject.getProcTs()));
        return DalyrejsRecordLayout.of(daily, reject.getReasonCode(), reject.getReasonDesc());
    }

    /**
     * TRANSACT / SYSTRAN. Posted transactions carry the DALYTRAN-ORIG-TS they were copied from;
     * {@code interest} marks a transaction INTCALC wrote, which carries the DB2 timestamp it stamped.
     */
    static CobolRecord toTranRecord(Transaction t, CodePage codePage, boolean interest) {
        boolean origFromDalytran = !interest;
        return CobolRecord.initialized(TranRecordLayout.LAYOUT, codePage)
                .set(TranRecordLayout.TRAN_ID, t.getTranId())
                .set(TranRecordLayout.TRAN_TYPE_CD, t.getTypeCd())
                .set(TranRecordLayout.TRAN_CAT_CD, number(t.getCatCd()))
                .set(TranRecordLayout.TRAN_SOURCE, t.getSource())
                .set(TranRecordLayout.TRAN_DESC, t.getDescription())
                .set(TranRecordLayout.TRAN_AMT, amount(t.getAmount()))
                .set(TranRecordLayout.TRAN_MERCHANT_ID, number(t.getMerchantId()))
                .set(TranRecordLayout.TRAN_MERCHANT_NAME, t.getMerchantName())
                .set(TranRecordLayout.TRAN_MERCHANT_CITY, t.getMerchantCity())
                .set(TranRecordLayout.TRAN_MERCHANT_ZIP, t.getMerchantZip())
                .set(TranRecordLayout.TRAN_CARD_NUM, t.getCardNum())
                .set(TranRecordLayout.TRAN_ORIG_TS,
                        origFromDalytran ? fileTimestamp(t.getOrigTs()) : CobolValues.toDb2Timestamp(t.getOrigTs()))
                .set(TranRecordLayout.TRAN_PROC_TS, CobolValues.toDb2Timestamp(t.getProcTs()));
    }

    static CobolRecord toCategoryBalanceRecord(CategoryBalance balance, CodePage codePage) {
        return CobolRecord.initialized(TranCatBalRecordLayout.LAYOUT, codePage)
                .set(TranCatBalRecordLayout.TRANCAT_ACCT_ID, number(balance.getId().getAcctId()))
                .set(TranCatBalRecordLayout.TRANCAT_TYPE_CD, balance.getId().getTypeCd())
                .set(TranCatBalRecordLayout.TRANCAT_CD, number(balance.getId().getCatCd()))
                .set(TranCatBalRecordLayout.TRAN_CAT_BAL, amount(balance.getBalance()));
    }

    private static String fileTimestamp(LocalDateTime value) {
        return value == null ? null : FILE_TIMESTAMP.format(value);
    }

    private static long number(Number value) {
        return value == null ? 0L : value.longValue();
    }

    private static BigDecimal amount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }
}
