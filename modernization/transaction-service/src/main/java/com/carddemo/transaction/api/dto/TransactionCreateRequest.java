package com.carddemo.transaction.api.dto;

/**
 * Input fields of the COTRN02C add screen (map COTRN2A). Every field is text, as on the 3270 map,
 * so the edits of {@code VALIDATE-INPUT-KEY-FIELDS} / {@code VALIDATE-INPUT-DATA-FIELDS} can answer
 * with the COBOL messages ({@code TransactionAddValidator}); JSON numbers are accepted too.
 * Either {@code accountId} or {@code cardNumber} identifies the card; when both are sent the account
 * wins, as in the COBOL {@code EVALUATE}. The request itself is the {@code Y} confirmation.
 */
public record TransactionCreateRequest(
        String accountId,
        String cardNumber,
        String typeCode,
        String categoryCode,
        String source,
        String description,
        String amount,
        String originDate,
        String processDate,
        String merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip) {
}
