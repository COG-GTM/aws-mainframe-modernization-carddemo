package com.carddemo.transactiontype.api;

import com.carddemo.transactiontype.domain.LegacyRuleException;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.CannotGetJdbcConnectionException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
public class ApiExceptionHandler {

    @ExceptionHandler(LegacyRuleException.class)
    ResponseEntity<ApiError> rule(LegacyRuleException e) {
        HttpStatus status = switch (e.kind()) {
            case VALIDATION -> HttpStatus.BAD_REQUEST;
            case NO_CHANGE -> HttpStatus.UNPROCESSABLE_ENTITY;
            case NOT_FOUND -> HttpStatus.NOT_FOUND;
            case CONFLICT, LOCKED -> HttpStatus.CONFLICT;
            case CONFIRMATION_REQUIRED -> HttpStatus.PRECONDITION_REQUIRED;
            case DB_ERROR -> HttpStatus.INTERNAL_SERVER_ERROR;
        };
        return ResponseEntity.status(status)
                .body(new ApiError(status.value(), status.getReasonPhrase(), e.ruleId(), e.getMessage(), e.sqlCode()));
    }

    /** BR-TT-061: 9998-PRIMING-QUERY - no Db2 access, quit with an error instead of serving data. */
    @ExceptionHandler({CannotGetJdbcConnectionException.class, DataAccessResourceFailureException.class})
    ResponseEntity<ApiError> unavailable(RuntimeException e) {
        HttpStatus s = HttpStatus.SERVICE_UNAVAILABLE;
        return ResponseEntity.status(s).body(new ApiError(s.value(), s.getReasonPhrase(), "BR-TT-061",
                "Db2 is not reachable: " + e.getClass().getSimpleName(), null));
    }
}
