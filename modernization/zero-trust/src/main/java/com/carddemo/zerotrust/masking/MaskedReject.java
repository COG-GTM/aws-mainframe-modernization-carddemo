package com.carddemo.zerotrust.masking;

import java.math.BigDecimal;

/** A reject record rendered safe for display: the card number is already masked. */
public record MaskedReject(String transactionId, String maskedCard, BigDecimal amount,
                           int reasonCode, String reasonText) {
}
