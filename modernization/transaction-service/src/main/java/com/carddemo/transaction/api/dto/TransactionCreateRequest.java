package com.carddemo.transaction.api.dto;

import jakarta.validation.constraints.Digits;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
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
        @NotNull @Min(0) @Max(9999) Integer categoryCode,
        @Size(max = 10) String source,
        @Size(max = 100) String description,
        @NotNull @Digits(integer = 9, fraction = 2) BigDecimal amount,
        @Min(0) @Max(999_999_999) Long merchantId,
        @Size(max = 50) String merchantName,
        @Size(max = 50) String merchantCity,
        @Size(max = 10) String merchantZip,
        LocalDateTime originTimestamp) {
}
