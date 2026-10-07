package com.carddemo.web.card;

import io.swagger.v3.oas.annotations.media.Schema;

/** One {@code CRDSEL}/{@code ACCTNO}/{@code CRDNUM}/{@code CRDSTS} row of map CCRDLIA. */
@Schema(description = "One list row; the card number is masked (ADR-0020), use cardRef to address the card")
public record CardListRow(
        @Schema(description = "Row 1..7 on the screen", example = "1") int row,
        @Schema(description = "ACCTNOn: CARD-ACCT-ID, 11 digits", example = "00000000050") String accountId,
        @Schema(description = "CRDNUMn, masked: only the last four digits", example = "************5740")
        String cardNumber,
        @Schema(description = "CRDSTSn: CARD-ACTIVE-STATUS Y/N", example = "Y") String activeStatus,
        @Schema(description = "Opaque reference to the card: usable as {cardNumber} in the card endpoints and as "
                + "after/before cursor", example = "kV3t2b6mV0sWZ0lq8nO4bQ") String cardRef) {
}
