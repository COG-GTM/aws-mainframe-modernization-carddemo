package com.carddemo.account.api.dto;

import com.carddemo.account.service.PostingOutcome;
import java.math.BigDecimal;

/**
 * Result of a posting attempt. {@code reasonCode} carries the CBTRN02C
 * {@code WS-VALIDATION-FAIL-REASON} value (101, 102, 103) so the batch job can write the same
 * reject records the mainframe wrote to DALYREJS.
 */
public record PostingResponse(
        boolean posted,
        Integer reasonCode,
        String reasonDescription,
        BigDecimal currentBalance,
        BigDecimal currentCycleCredit,
        BigDecimal currentCycleDebit) {

    public static PostingResponse from(PostingOutcome outcome) {
        return new PostingResponse(
                outcome.posted(),
                outcome.reasonCode(),
                outcome.reasonDescription(),
                outcome.currentBalance(),
                outcome.currentCycleCredit(),
                outcome.currentCycleDebit());
    }
}
