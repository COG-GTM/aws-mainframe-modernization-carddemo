package com.carddemo.card.api.dto;

import com.carddemo.card.service.CardMessages;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * The editable fields of the COCRDUP map (CRDNAME, CRDSTCD, EXPMON, EXPYEAR) with the COCRDUPC edits
 * 1230-EDIT-NAME .. 1260-EDIT-EXPIRY-YEAR as constraints. The expiry day is not on the screen (EXPDAY
 * is a dark protected field) and the CVV is not either, so neither can be changed. {@code version}
 * is the record version the client read: the copy of the record COCRDUPC keeps in its COMMAREA
 * (CCUP-OLD-DETAILS) to detect a change by someone else before the rewrite.
 */
public record CardUpdateRequest(
        @NotBlank(message = CardMessages.NAME_NOT_PROVIDED)
        @Pattern(regexp = "[A-Za-z ]*", message = CardMessages.NAME_MUST_BE_ALPHA)
        @Size(max = 50) String embossedName,
        @NotNull(message = CardMessages.STATUS_MUST_BE_YES_NO)
        @Pattern(regexp = "[YN]", message = CardMessages.STATUS_MUST_BE_YES_NO) String activeStatus,
        @NotNull(message = CardMessages.EXPIRY_MONTH_NOT_VALID)
        @Min(value = 1, message = CardMessages.EXPIRY_MONTH_NOT_VALID)
        @Max(value = 12, message = CardMessages.EXPIRY_MONTH_NOT_VALID) Integer expirationMonth,
        @NotNull(message = CardMessages.EXPIRY_YEAR_NOT_VALID)
        @Min(value = 1950, message = CardMessages.EXPIRY_YEAR_NOT_VALID)
        @Max(value = 2099, message = CardMessages.EXPIRY_YEAR_NOT_VALID) Integer expirationYear,
        @NotNull Long version) {
}
