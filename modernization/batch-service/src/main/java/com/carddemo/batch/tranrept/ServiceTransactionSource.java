package com.carddemo.batch.tranrept;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.client.RestClient;

/**
 * STEP05R REPROC without a TRANSACT file: unloads the TRANSACT KSDS as it is now from
 * transaction-service ({@code GET /api/v1/transactions}, in TRAN-ID order) into TRAN-RECORDs; a
 * transaction added or removed while the pages are read makes {@link ServicePages} read again.
 * Timestamps are written as {@code YYYY-MM-DD HH:MM:SS.ffffff}; a POSTTRAN dump keeps the DB2 layout
 * ({@code YYYY-MM-DD-HH.MM.SS.ffffff}) for some rows, which CBTRN03C never looks at past
 * {@code TRAN-PROC-TS(1:10)}.
 */
public final class ServiceTransactionSource {

    static final DateTimeFormatter TIMESTAMP = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private final RestClient transactions;

    public ServiceTransactionSource(RestClient transactionRestClient) {
        this.transactions = transactionRestClient;
    }

    record TransactionView(String transactionId, String cardNumber, String typeCode, Integer categoryCode,
                           String source, String description, BigDecimal amount, String merchantId,
                           String merchantName, String merchantCity, String merchantZip,
                           LocalDateTime originTimestamp, LocalDateTime processTimestamp) {
    }

    public List<CobolRecord> unload(CodePage codePage) {
        return ServicePages.all(transactions, "/api/v1/transactions",
                        new ParameterizedTypeReference<ServicePages.Page<TransactionView>>() { },
                        TransactionView::transactionId)
                .stream().map(t -> toRecord(t, codePage)).toList();
    }

    static CobolRecord toRecord(TransactionView t, CodePage codePage) {
        return CobolRecord.initialized(TranRecordLayout.LAYOUT, codePage)
                .set(TranRecordLayout.TRAN_ID, t.transactionId())
                .set(TranRecordLayout.TRAN_TYPE_CD, t.typeCode())
                .set(TranRecordLayout.TRAN_CAT_CD, t.categoryCode() == null ? 0 : t.categoryCode())
                .set(TranRecordLayout.TRAN_SOURCE, t.source())
                .set(TranRecordLayout.TRAN_DESC, t.description())
                .set(TranRecordLayout.TRAN_AMT, t.amount() == null ? BigDecimal.ZERO : t.amount())
                .set(TranRecordLayout.TRAN_MERCHANT_ID, t.merchantId() == null || t.merchantId().isBlank()
                        ? 0 : Long.parseLong(t.merchantId().strip()))
                .set(TranRecordLayout.TRAN_MERCHANT_NAME, t.merchantName())
                .set(TranRecordLayout.TRAN_MERCHANT_CITY, t.merchantCity())
                .set(TranRecordLayout.TRAN_MERCHANT_ZIP, t.merchantZip())
                .set(TranRecordLayout.TRAN_CARD_NUM, t.cardNumber())
                .set(TranRecordLayout.TRAN_ORIG_TS, timestamp(t.originTimestamp()))
                .set(TranRecordLayout.TRAN_PROC_TS, timestamp(t.processTimestamp()));
    }

    private static String timestamp(LocalDateTime value) {
        return value == null ? null : TIMESTAMP.format(value);
    }
}
