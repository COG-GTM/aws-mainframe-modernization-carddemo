package com.carddemo.recordio.layout;

import java.math.BigDecimal;

/**
 * One transaction, as carried in both the daily input file ({@code CVTRA06Y.cpy DALYTRAN-RECORD})
 * and the transaction master ({@code CVTRA05Y.cpy TRAN-RECORD}). The two copybooks are
 * field-for-field identical (350 bytes); only the level-05 names differ.
 *
 * <p>Timestamps are kept as the 26-character {@code X(26)} text the programs compare and slice
 * ({@code TS(1:10)}), not parsed, because the COBOL never validates them.
 */
public record Transaction(
        String id,
        String typeCode,
        int categoryCode,
        String source,
        String description,
        BigDecimal amount,
        long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNumber,
        String originalTimestamp,
        String processingTimestamp) {

    public static final int LENGTH = 350;

    /** {@code TS(1:10)} — the yyyy-mm-dd prefix of a DB2-style timestamp. */
    public String originalDate() {
        return originalTimestamp.substring(0, 10);
    }

    public String processingDate() {
        return processingTimestamp.substring(0, 10);
    }

    public Transaction withProcessingTimestamp(String timestamp) {
        return new Transaction(id, typeCode, categoryCode, source, description, amount, merchantId,
                merchantName, merchantCity, merchantZip, cardNumber, originalTimestamp, timestamp);
    }
}
