package com.carddemo.common.error;

import com.carddemo.common.api.ApiError.FieldProblem;
import java.util.List;

/**
 * Screen edits that failed, found by service code rather than Bean Validation: rendered as 400 with
 * the first problem's message as {@code ApiError.message} (the COBOL shows only the first message)
 * and every problem in {@code fieldErrors}.
 */
public class FieldValidationException extends RuntimeException {

    private final transient List<FieldProblem> problems;

    public FieldValidationException(List<FieldProblem> problems) {
        super(problems.get(0).message());
        this.problems = List.copyOf(problems);
    }

    public FieldValidationException(String field, String message) {
        this(List.of(new FieldProblem(field, message)));
    }

    public List<FieldProblem> problems() {
        return problems;
    }
}
