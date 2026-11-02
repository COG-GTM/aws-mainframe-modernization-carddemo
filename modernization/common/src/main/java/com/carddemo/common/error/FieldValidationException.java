package com.carddemo.common.error;

import com.carddemo.common.api.ApiError;
import java.util.List;

/**
 * Screen-level field edits done in code rather than by Bean Validation, because their order and text
 * come from the COBOL program. The first problem is the field the COBOL would have put the cursor on
 * ({@code MOVE -1 TO xxxL}); its message becomes {@code ApiError.message}.
 */
public class FieldValidationException extends RuntimeException {

    private final List<ApiError.FieldProblem> problems;

    public FieldValidationException(List<ApiError.FieldProblem> problems) {
        super(problems.get(0).message());
        this.problems = List.copyOf(problems);
    }

    public List<ApiError.FieldProblem> problems() {
        return problems;
    }
}
