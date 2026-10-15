package com.carddemo.transaction;

import java.math.BigDecimal;

/** confirmationRequired mirrors the COBOL two-step "Confirm ... (Y/N)" pattern without server-side session state. */
public record OperationResult(String message, boolean confirmationRequired, String tranId, BigDecimal currentBalance) {
}
