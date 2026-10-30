package com.carddemo.transactiontype.domain;

import java.util.List;

/**
 * One COTRTLIC screen worth of rows (BR-TT-010). {@code nextStartKey} is the key of the first row on
 * the next page (the 8th row peeked by 8000-READ-FORWARD); {@code firstKey} is used for PF7.
 */
public record TransactionTypePage(
        List<TransactionType> items,
        String firstKey,
        String nextStartKey,
        boolean hasNextPage,
        boolean hasPreviousPage,
        long totalMatching,
        String message) {
}
