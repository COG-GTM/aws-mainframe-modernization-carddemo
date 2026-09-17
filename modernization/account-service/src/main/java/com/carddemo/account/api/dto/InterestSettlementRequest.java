package com.carddemo.account.api.dto;

import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;

/** Account break of CBACT04C paragraph 1050-UPDATE-ACCOUNT: add the cycle interest, reset the cycle. */
public record InterestSettlementRequest(@NotNull BigDecimal totalInterest) {
}
