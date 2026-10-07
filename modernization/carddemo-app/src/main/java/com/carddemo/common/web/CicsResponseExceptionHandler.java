package com.carddemo.common.web;

import com.carddemo.common.AbendException;
import com.carddemo.common.DuplicateRecordException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.RecordNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ProblemDetail;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * Maps the CICS RESP conditions the online programs test (ADR-0009) and record-changed conflicts (ADR-0010)
 * to RFC 7807 problem responses. The {@code cicsResp} property keeps the original condition name so the UI and
 * parity tests can assert on it.
 */
@RestControllerAdvice
public class CicsResponseExceptionHandler {

    private static final Logger log = LoggerFactory.getLogger(CicsResponseExceptionHandler.class);

    @ExceptionHandler(RecordNotFoundException.class)
    ProblemDetail notFound(RecordNotFoundException e) {
        return problem(HttpStatus.NOT_FOUND, e.getMessage(), "NOTFND");
    }

    @ExceptionHandler(DuplicateRecordException.class)
    ProblemDetail duplicate(DuplicateRecordException e) {
        return problem(HttpStatus.CONFLICT, e.getMessage(), "DUPREC");
    }

    @ExceptionHandler(InvalidRequestException.class)
    ProblemDetail invalid(InvalidRequestException e) {
        return problem(HttpStatus.BAD_REQUEST, e.getMessage(), "INVREQ");
    }

    @ExceptionHandler(ObjectOptimisticLockingFailureException.class)
    ProblemDetail changedBeforeUpdate(ObjectOptimisticLockingFailureException e) {
        return problem(HttpStatus.CONFLICT, "Record changed by some one else. Please review", "CHANGED");
    }

    @ExceptionHandler(AbendException.class)
    ProblemDetail abend(AbendException e) {
        log.error(e.getMessage(), e);
        ProblemDetail problem = problem(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage(), "ABEND");
        problem.setProperty("abendCode", e.abendLabel());
        return problem;
    }

    private static ProblemDetail problem(HttpStatus status, String detail, String cicsResp) {
        ProblemDetail problem = ProblemDetail.forStatusAndDetail(status, detail);
        problem.setProperty("cicsResp", cicsResp);
        return problem;
    }
}
