package com.carddemo.web.card;

import com.carddemo.card.online.CardChanges;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/** The fields of map CCRDUPA as typed, the version that was displayed (ADR-0010) and the PF5 confirmation. */
@Schema(description = "CCRDUPA fields as text (the server applies the COCRDUPC edits), the card version that was "
        + "displayed and the PF5 confirmation. Blank or '*' means not supplied.")
public record CardUpdateRequest(
        @Schema(description = "ACCTSID search key (protected once details are fetched; required)",
                example = "00000000050") @Size(max = 11) String accountId,
        @Schema(description = "CARD version from the GET (optimistic lock)", example = "0")
        @NotNull(message = "version must be supplied.") Long version,
        @Schema(description = "false = ENTER (validate only, state N 'Changes validated.Press F5 to save'); true = "
                + "PF5 (validate and REWRITE)", example = "false", defaultValue = "false") Boolean confirm,
        @Schema(description = "CRDNAME: letters and spaces only", example = "ANIYA VON") @Size(max = 50)
        String embossedName,
        @Schema(description = "CRDSTCD: Y or N (upper case)", example = "Y") @Size(max = 1) String activeStatus,
        @Schema(description = "EXPMON: 1..12", example = "03") @Size(max = 2) String expiryMonth,
        @Schema(description = "EXPYEAR: 1950..2099", example = "2023") @Size(max = 4) String expiryYear) {

    CardChanges toChanges() {
        return new CardChanges(embossedName, activeStatus, expiryMonth, expiryYear);
    }

    boolean confirmed() {
        return Boolean.TRUE.equals(confirm);
    }
}
