package com.carddemo.card.api.dto;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import java.time.LocalDate;

/** Update payload of COCRDUPC; the edit rules of the COCRDUP map become constraints. */
public record CardUpdateRequest(
        @NotBlank @Size(max = 50) @Pattern(regexp = "[A-Za-z .'-]+", message = "embossed name must be alphabetic")
        String embossedName,
        @NotNull LocalDate expirationDate,
        @NotNull @Pattern(regexp = "[YNyn]", message = "active status must be Y or N") String activeStatus,
        @Min(0) @Max(999) Integer cvv) {
}
