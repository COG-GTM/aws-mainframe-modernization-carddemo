package com.carddemo.common.web;

import com.carddemo.common.AbendException;
import com.carddemo.common.DuplicateRecordException;
import com.carddemo.common.FieldEditException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import com.carddemo.common.online.CommonMessages;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ProblemDetail;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

/**
 * Maps the CICS RESP conditions the online programs test (ADR-0009) and record-changed conflicts (ADR-0010)
 * to RFC 7807 problem responses carrying the uniform {@code code}/{@code field}/{@code message} members (ADR-0019).
 * The {@code cicsResp} property keeps the original condition name so the UI and parity tests can assert on it.
 * Spring MVC's own errors (bad JSON, failed bean validation, unsupported method, unknown path) get the same shape.
 */
@RestControllerAdvice
public class CicsResponseExceptionHandler extends ResponseEntityExceptionHandler {

    private static final Logger log = LoggerFactory.getLogger(CicsResponseExceptionHandler.class);

    /** Code of an HTTP method a resource does not support: the BMS equivalent of an AID the program ignores. */
    public static final String INVALID_KEY = "INVALID_KEY";

    @ExceptionHandler(RecordNotFoundException.class)
    ProblemDetail notFound(RecordNotFoundException e) {
        return problem(HttpStatus.NOT_FOUND, null, e.getMessage(), "NOTFND");
    }

    @ExceptionHandler(DuplicateRecordException.class)
    ProblemDetail duplicate(DuplicateRecordException e) {
        return problem(HttpStatus.CONFLICT, null, e.getMessage(), e.condition().name());
    }

    @ExceptionHandler(InvalidRequestException.class)
    ProblemDetail invalid(InvalidRequestException e) {
        ProblemDetail problem = problem(HttpStatus.BAD_REQUEST, e.field(), e.getMessage(), "INVREQ");
        if (e instanceof FieldEditException edit) {
            problem.setProperty("invalidFields", edit.invalidFields());
        }
        return problem;
    }

    @ExceptionHandler(ObjectOptimisticLockingFailureException.class)
    ProblemDetail changedBeforeUpdate(ObjectOptimisticLockingFailureException e) {
        return problem(HttpStatus.CONFLICT, null, "Record changed by some one else. Please review", "CHANGED");
    }

    @ExceptionHandler(AbendException.class)
    ProblemDetail abend(AbendException e) {
        log.error(e.getMessage(), e);
        ProblemDetail problem = problem(HttpStatus.INTERNAL_SERVER_ERROR, null, e.getMessage(), "ABEND");
        problem.setProperty("abendCode", e.abendLabel());
        return problem;
    }

    @Override
    protected ResponseEntity<Object> handleMethodArgumentNotValid(MethodArgumentNotValidException ex,
            HttpHeaders headers, HttpStatusCode status, WebRequest request) {
        FieldError error = ex.getBindingResult().getFieldError();
        String field = error == null ? null : error.getField();
        String message = error == null ? "Request is not valid" : error.getDefaultMessage();
        return handleExceptionInternal(ex, problem(HttpStatus.BAD_REQUEST, field, message, "INVREQ"), headers,
                HttpStatus.BAD_REQUEST, request);
    }

    @Override
    protected ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException ex,
            HttpHeaders headers, HttpStatusCode status, WebRequest request) {
        return handleExceptionInternal(ex,
                problem(HttpStatus.BAD_REQUEST, null, "Request body is missing or is not valid JSON", "INVREQ"),
                headers, HttpStatus.BAD_REQUEST, request);
    }

    @Override
    protected ResponseEntity<Object> handleHttpRequestMethodNotSupported(HttpRequestMethodNotSupportedException ex,
            HttpHeaders headers, HttpStatusCode status, WebRequest request) {
        ResponseEntity<Object> response = super.handleHttpRequestMethodNotSupported(ex, headers, status, request);
        if (response != null && response.getBody() instanceof ProblemDetail problem) {
            ApiErrors.decorate(problem, INVALID_KEY, null, CommonMessages.INVALID_KEY);
        }
        return response;
    }

    @Override
    protected ResponseEntity<Object> handleExceptionInternal(Exception ex, Object body, HttpHeaders headers,
            HttpStatusCode statusCode, WebRequest request) {
        ResponseEntity<Object> response = super.handleExceptionInternal(ex, body, headers, statusCode, request);
        if (response != null && response.getBody() instanceof ProblemDetail problem
                && !ApiErrors.isDecorated(problem)) {
            ApiErrors.decorate(problem, defaultCode(statusCode), null,
                    problem.getDetail() == null ? problem.getTitle() : problem.getDetail());
        }
        return response;
    }

    private static String defaultCode(HttpStatusCode status) {
        return switch (status.value()) {
            case 400 -> "INVREQ";
            case 404 -> "NOTFND";
            case 405 -> INVALID_KEY;
            default -> "HTTP_" + status.value();
        };
    }

    private static ProblemDetail problem(HttpStatus status, String field, String detail, String cicsResp) {
        ProblemDetail problem = ApiErrors.problem(status, cicsResp, field, detail);
        problem.setProperty("cicsResp", cicsResp);
        return problem;
    }
}
