package com.carddemo.transaction.online;

/**
 * The input fields of COTRN2A ({@code ACTIDIN} .. {@code MZIPIN}) as typed, or as normalised by the edits (account
 * and card resolved, amount re-edited as {@code +99999999.99}).
 */
public record TransactionForm(
        String accountId,
        String cardNumber,
        String typeCode,
        String categoryCode,
        String source,
        String description,
        String amount,
        String origDate,
        String procDate,
        String merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip) {

    TransactionForm withKeys(String accountId, String cardNumber) {
        return new TransactionForm(accountId, cardNumber, typeCode, categoryCode, source, description, amount,
                origDate, procDate, merchantId, merchantName, merchantCity, merchantZip);
    }

    TransactionForm withAmount(String amount) {
        return new TransactionForm(accountId, cardNumber, typeCode, categoryCode, source, description, amount,
                origDate, procDate, merchantId, merchantName, merchantCity, merchantZip);
    }
}
