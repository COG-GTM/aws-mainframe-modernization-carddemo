package com.carddemo.web.transaction;

import com.carddemo.web.NavigationContext;
import com.carddemo.web.ScreenHeader;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;

/** COTRN0A: ten transactions per page. */
@Schema(description = "Transaction list screen (COTRN00C / map COTRN0A)")
public record TransactionListScreen(
        ScreenHeader header,
        @Schema(description = "PAGENUM (CDEMO-CT00-PAGE-NUM); send it back as page= with PF7/PF8; 0 when no rows",
                example = "1") int pageNumber,
        @Schema(description = "Rows per page", example = "10") int pageSize,
        List<TransactionListRow> rows,
        @Schema(description = "PF7 would show an earlier page (PAGE-NUM > 1)") boolean hasPreviousPage,
        @Schema(description = "PF8 would show a later page (NEXT-PAGE-YES)") boolean hasNextPage,
        @Schema(description = "PF7: pass as before= (first id shown); null when there is no previous page",
                nullable = true, example = "0000000000000011") String previousPage,
        @Schema(description = "PF8: pass as after= (last id shown); null when there is no next page",
                nullable = true, example = "0000000000000020") String nextPage,
        @Schema(description = "ERRMSG", example = "") String message,
        @Schema(description = "PF3: COMEN01C") NavigationContext exit) {
}
