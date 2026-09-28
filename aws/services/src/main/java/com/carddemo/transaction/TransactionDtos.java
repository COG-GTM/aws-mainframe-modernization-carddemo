package com.carddemo.transaction;

import java.time.LocalDateTime;

public final class TransactionDtos {

    private TransactionDtos() {
    }

    public record TransactionSummary(String tranId, String origDate, String description, String amt) {
    }

    public record TransactionDetail(String tranId, String cardNum, String typeCd, int catCd, String source,
            String description, String amt, LocalDateTime origTs, LocalDateTime procTs, Integer merchantId,
            String merchantName, String merchantCity, String merchantZip) {
    }

    /** POST body; scalar values are accepted as JSON strings or numbers so legacy numeric edits apply. */
    public record CreateTransactionRequest(String acctId, String cardNum, String typeCd, String catCd, String source,
            String description, String amt, String origDate, String procDate, String merchantId, String merchantName,
            String merchantCity, String merchantZip) {
    }

    public record CreateTransactionResponse(String tranId, String message) {
    }
}
