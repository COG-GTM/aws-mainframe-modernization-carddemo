package com.carddemo.web.menu;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/** {@code OPTIONI PIC X(2)}: the option number as typed ({@code "1"}, {@code " 1"}, {@code "01"}). */
@Schema(description = "Selected option (OPTIONI, 2 characters)")
public record MenuSelectionRequest(
        @Schema(example = "1", maxLength = 2) @Size(max = 2, message = "Option can be at most 2 characters")
        String option) {
}
