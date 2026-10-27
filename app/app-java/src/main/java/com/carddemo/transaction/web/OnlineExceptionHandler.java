package com.carddemo.transaction.web;

import com.fasterxml.jackson.annotation.JsonInclude;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@RestControllerAdvice
public class OnlineExceptionHandler {

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record ErrorResponse(String message, String accountId, String cardNumber) {
    }

    @ExceptionHandler(OnlineException.class)
    public ResponseEntity<ErrorResponse> handle(OnlineException e) {
        return ResponseEntity.status(e.getStatus())
                .body(new ErrorResponse(e.getMessage(), e.getAccountId(), e.getCardNumber()));
    }
}
