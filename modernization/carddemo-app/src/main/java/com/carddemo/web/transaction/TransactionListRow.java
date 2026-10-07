package com.carddemo.web.transaction;

import io.swagger.v3.oas.annotations.media.Schema;

/** One line of COTRN0A ({@code TRNIDnn}, {@code TDATEnn}, {@code TDESCnn}, {@code TAMTnnn}). */
@Schema(description = "A transaction row of the list (COTRN00C R-17)")
public record TransactionListRow(
        @Schema(description = "Row number on the page (1..10)", example = "1") int row,
        @Schema(description = "TRAN-ID", example = "0000000000000001") String tranId,
        @Schema(description = "TRAN-ORIG-TS as MM/DD/YY", example = "07/06/22") String date,
        @Schema(description = "TRAN-DESC, first 26 characters", example = "BILL PAYMENT - ONLINE") String description,
        @Schema(description = "TRAN-AMT edited +99999999.99", example = "+00000045.10") String amount) {
}
