package com.carddemo.web;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * {@code POPULATE-HEADER-INFO}: the header fields every map sends (COSGN00C R-15, COMEN01C R-13, COADM01C R-12).
 *
 * @param currentDate {@code CURDATE} as {@code MM/DD/YY}
 * @param currentTime {@code CURTIME} as {@code HH:MM:SS}
 */
@Schema(description = "Screen header (POPULATE-HEADER-INFO)")
public record ScreenHeader(
        @Schema(example = "AWS Mainframe Modernization") String title01,
        @Schema(example = "CardDemo") String title02,
        @Schema(example = "CC00") String tranId,
        @Schema(example = "COSGN00C") String programName,
        @Schema(example = "07/06/22") String currentDate,
        @Schema(example = "13:45:10") String currentTime,
        @Schema(example = "CARDDEMO") String applId,
        @Schema(example = "CDMO") String sysId) {
}
