package com.carddemo.transaction.api.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/** COTRN02C add transaction map. */
public record TransactionCreateRequest(
        @NotBlank @Pattern(regexp = "\\d{16}", message = "card number must be 16 digits") String cardNumber,
        @NotBlank @Size(max = 2) String typeCode,
        @NotNull Integer categoryCode,
        @Size(max = 10) String source,
        @Size(max = 100) String description,
        @NotNull BigDecimal amount,
        Long merchantId,
        @Size(max = 50) String merchantName,
        @Size(max = 50) String merchantCity,
        @Size(max = 10) String merchantZip,
        LocalDateTime originTimestamp) {
}
