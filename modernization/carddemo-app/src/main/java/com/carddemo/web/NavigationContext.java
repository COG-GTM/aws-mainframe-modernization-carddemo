package com.carddemo.web;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;

/**
 * The navigation part of {@code CARDDEMO-COMMAREA} ({@code COCOM01Y}) that one screen hands to the next (ADR-0007,
 * ADR-0019): returned by an endpoint that performs an {@code XCTL}, and sent back by the UI to the target screen.
 * The user id and type are deliberately absent: they come from the authenticated token only.
 *
 * @param fromTranId  {@code CDEMO-FROM-TRANID}
 * @param fromProgram {@code CDEMO-FROM-PROGRAM}
 * @param toTranId    {@code CDEMO-TO-TRANID}
 * @param toProgram   {@code CDEMO-TO-PROGRAM}
 * @param pgmContext  {@code CDEMO-PGM-CONTEXT}
 * @param custId      {@code CDEMO-CUST-ID PIC 9(09)}
 * @param acctId      {@code CDEMO-ACCT-ID PIC 9(11)}
 * @param cardNum     {@code CDEMO-CARD-NUM PIC 9(16)}
 */
@Schema(description = "COMMAREA navigation fields handed to the next screen (COCOM01Y); user id/type come from the "
        + "token, never from here")
public record NavigationContext(
        @Schema(example = "CM00") String fromTranId,
        @Schema(example = "COMEN01C") String fromProgram,
        @Schema(example = "CAVW", nullable = true) String toTranId,
        @Schema(example = "COACTVWC") String toProgram,
        @Schema(example = "ENTER") ProgramContext pgmContext,
        @Schema(example = "1", nullable = true) @Min(0) @Max(999_999_999L) Integer custId,
        @Schema(example = "1", nullable = true) @Min(0) @Max(99_999_999_999L) Long acctId,
        @Schema(example = "0500024453765740", nullable = true) @Pattern(regexp = "\\d{16}") String cardNum) {

    /** {@code XCTL} from one program to another with {@code CDEMO-PGM-CONTEXT = 0} and no selection. */
    public static NavigationContext transfer(String fromTranId, String fromProgram, String toTranId,
            String toProgram) {
        return new NavigationContext(fromTranId, fromProgram, toTranId, toProgram, ProgramContext.ENTER, null, null,
                null);
    }

    /** The same navigation carrying the selected customer/account/card for the next screen. */
    public NavigationContext withSelection(Integer custId, Long acctId, String cardNum) {
        return new NavigationContext(fromTranId, fromProgram, toTranId, toProgram, pgmContext, custId, acctId,
                cardNum);
    }
}
