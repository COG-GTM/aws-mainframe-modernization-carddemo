package com.carddemo.transaction.client;

import java.math.BigDecimal;

/** Response of POST /api/v1/accounts/{id}/postings. */
public record PostingResult(
        boolean posted,
        Integer reasonCode,
        String reasonDescription,
        BigDecimal currentBalance,
        BigDecimal currentCycleCredit,
        BigDecimal currentCycleDebit) {
}
