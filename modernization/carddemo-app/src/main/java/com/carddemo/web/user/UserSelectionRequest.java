package com.carddemo.web.user;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.util.List;

/** The {@code SEL0001..SEL0010} codes typed on the rows of the page shown, with ENTER. */
@Schema(description = "Selection codes of the rows shown (SEL0001..SEL0010): U = update, D = delete, blank = none")
public record UserSelectionRequest(
        @NotNull(message = "rows must be supplied.") @Size(max = 10, message = "At most 10 rows are shown.")
        List<@Valid @NotNull Row> rows) {

    public record Row(
            @Schema(description = "USRIDnn of the row", example = "USER0001") @NotBlank @Size(max = 8) String userId,
            @Schema(description = "SELnnnn: U/u, D/d or blank", example = "U") @Size(max = 1) String selection) {
    }
}
