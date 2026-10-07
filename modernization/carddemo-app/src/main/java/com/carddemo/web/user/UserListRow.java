package com.carddemo.web.user;

import io.swagger.v3.oas.annotations.media.Schema;

/** One row of COUSR0A: {@code USRIDnn}, {@code FNAMEnn}, {@code LNAMEnn}, {@code UTYPEnn} (COUSR00C R-19). */
@Schema(description = "User list row")
public record UserListRow(
        @Schema(description = "Row number 1..10", example = "1") int row,
        @Schema(description = "USRIDnn (SEC-USR-ID)", example = "ADMIN001") String userId,
        @Schema(description = "FNAMEnn (SEC-USR-FNAME)", example = "MARGARET") String firstName,
        @Schema(description = "LNAMEnn (SEC-USR-LNAME)", example = "GOLD") String lastName,
        @Schema(description = "UTYPEnn (SEC-USR-TYPE): A = admin, U = user", example = "A") String userType) {
}
