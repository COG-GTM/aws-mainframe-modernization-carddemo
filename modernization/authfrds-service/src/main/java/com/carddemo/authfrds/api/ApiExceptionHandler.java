package com.carddemo.authfrds.api;

import com.carddemo.authfrds.domain.LegacyRuleException;
import java.util.List;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.CannotGetJdbcConnectionException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
public class ApiExceptionHandler {

    @ExceptionHandler(LegacyRuleException.class)
    ResponseEntity<ApiError> rule(LegacyRuleException e) {
        HttpStatus status = switch (e.kind()) {
            case VALIDATION -> HttpStatus.BAD_REQUEST;
            case NOT_FOUND, END_OF_DATA -> HttpStatus.NOT_FOUND;
            case FRAUD_UPDATE_FAILED -> HttpStatus.UNPROCESSABLE_ENTITY;
            case IMS_ERROR -> HttpStatus.INTERNAL_SERVER_ERROR;
        };
        return ResponseEntity.status(status)
                .body(new ApiError(status.value(), status.getReasonPhrase(), e.ruleId(), e.getMessage(), e.sqlCode(), null));
    }

    /** BR-AF-011: the commarea PIC sizes are enforced before any SQL runs. */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    ResponseEntity<ApiError> invalid(MethodArgumentNotValidException e) {
        List<String> fields = e.getBindingResult().getFieldErrors().stream()
                .map(f -> f.getField() + ": " + f.getDefaultMessage()).sorted().toList();
        HttpStatus s = HttpStatus.BAD_REQUEST;
        return ResponseEntity.status(s).body(new ApiError(s.value(), s.getReasonPhrase(), "BR-AF-011",
                "request does not fit the COPAUS2C commarea", null, fields));
    }

    @ExceptionHandler({CannotGetJdbcConnectionException.class, DataAccessResourceFailureException.class})
    ResponseEntity<ApiError> unavailable(RuntimeException e) {
        HttpStatus s = HttpStatus.SERVICE_UNAVAILABLE;
        return ResponseEntity.status(s).body(new ApiError(s.value(), s.getReasonPhrase(), null,
                "Db2 is not reachable: " + e.getClass().getSimpleName(), null, null));
    }
}
