package com.carddemo.web.transaction;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.util.List;

/** The {@code SEL0001..SEL0010} codes typed on the rows of the page shown, with ENTER. */
@Schema(description = "Selection codes of the rows shown (SEL0001..SEL0010): S = view, blank = none")
public record TransactionSelectionRequest(
        @NotNull(message = "rows must be supplied.") @Size(max = 10, message = "At most 10 rows are shown.")
        List<@Valid @NotNull Row> rows) {

    public record Row(
            @Schema(description = "TRNIDnn of the row", example = "0000000000000001") @NotBlank
            @Size(max = 16) String tranId,
            @Schema(description = "SELnnnn: S/s or blank", example = "S") @Size(max = 1) String selection) {
    }
}
