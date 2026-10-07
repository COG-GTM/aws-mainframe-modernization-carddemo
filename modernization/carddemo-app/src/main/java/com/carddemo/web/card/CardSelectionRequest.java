package com.carddemo.web.card;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.util.List;

/** The {@code CRDSEL1..7} codes typed on the rows of the page shown, with ENTER. */
@Schema(description = "Selection codes of the rows shown (CRDSEL1..7): S = detail, U = update, blank = none")
public record CardSelectionRequest(
        @Schema(description = "ACCTSID of the account in context (the accountId of the list shown); required for a "
                + "USER, whose selected card must belong to it (ADR-0020)", example = "00000000050")
        String accountId,
        @NotNull(message = "rows must be supplied.") @Size(max = 7, message = "At most 7 rows are shown.")
        List<@Valid @NotNull Row> rows) {

    public record Row(
            @Schema(description = "cardRef of the row (from GET /api/v1/cards)") @NotBlank String cardRef,
            @Schema(description = "CRDSELn: S, U or blank", example = "S") @Size(max = 1) String action) {
    }
}
