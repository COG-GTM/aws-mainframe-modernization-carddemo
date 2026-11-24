package com.carddemo.transaction.web;

import org.springframework.http.HttpStatus;

/**
 * Equivalent of setting WS-ERR-FLG + WS-MESSAGE and re-sending the map: the request ends with the
 * screen's error message (verbatim from the COBOL program) and an HTTP status.
 */
public class OnlineException extends RuntimeException {

    private final HttpStatus status;
    private final String accountId;
    private final String cardNumber;

    public OnlineException(HttpStatus status, String message) {
        this(status, message, null, null);
    }

    public OnlineException(HttpStatus status, String message, String accountId, String cardNumber) {
        super(message);
        this.status = status;
        this.accountId = accountId;
        this.cardNumber = cardNumber;
    }

    public HttpStatus getStatus() { return status; }
    public String getAccountId() { return accountId; }
    public String getCardNumber() { return cardNumber; }
}
