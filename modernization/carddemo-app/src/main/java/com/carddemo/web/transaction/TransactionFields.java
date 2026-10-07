package com.carddemo.web.transaction;

import com.carddemo.transaction.Transaction;
import com.carddemo.transaction.online.TransactionFormat;
import io.swagger.v3.oas.annotations.media.Schema;

/** The 13 detail fields of COTRN1A (COTRN01C R-11). The card number is shown in full, as the map does (ADR-0020). */
@Schema(description = "A transaction as COTRN1A shows it")
public record TransactionFields(
        @Schema(description = "TRAN-ID", example = "0000000000000051") String tranId,
        @Schema(description = "TRAN-CARD-NUM (full, as on CARDNUM)", example = "0500024453765740") String cardNumber,
        @Schema(description = "TRAN-TYPE-CD", example = "02") String typeCode,
        @Schema(description = "TRAN-CAT-CD (4 digits)", example = "0002") String categoryCode,
        @Schema(description = "TRAN-SOURCE", example = "POS TERM") String source,
        @Schema(description = "TRAN-AMT edited +99999999.99", example = "+00000183.88") String amount,
        @Schema(description = "TRAN-DESC", example = "BILL PAYMENT - ONLINE") String description,
        @Schema(description = "TRAN-ORIG-TS", example = "2022-07-06 13:45:10.000000") String origTimestamp,
        @Schema(description = "TRAN-PROC-TS", example = "2022-07-06 13:45:10.000000") String procTimestamp,
        @Schema(description = "TRAN-MERCHANT-ID (9 digits)", example = "999999999") String merchantId,
        @Schema(description = "TRAN-MERCHANT-NAME", example = "BILL PAYMENT") String merchantName,
        @Schema(description = "TRAN-MERCHANT-CITY", example = "N/A") String merchantCity,
        @Schema(description = "TRAN-MERCHANT-ZIP", example = "N/A") String merchantZip) {

    public static TransactionFields of(Transaction t) {
        return new TransactionFields(t.getTranId(), t.getCardNum(), t.getTranTypeCd(),
                String.format("%04d", t.getTranCatCd()), t.getSource(), TransactionFormat.amount(t.getAmount()),
                t.getDescription(), t.getOrigTs(), t.getProcTs(), String.format("%09d", t.getMerchantId()),
                t.getMerchantName(), t.getMerchantCity(), t.getMerchantZip());
    }
}
