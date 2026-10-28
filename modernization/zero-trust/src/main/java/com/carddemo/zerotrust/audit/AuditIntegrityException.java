package com.carddemo.zerotrust.audit;

/** The audit chain failed verification; writers must fail closed rather than append. */
public class AuditIntegrityException extends RuntimeException {

    public AuditIntegrityException(String message) {
        super(message);
    }
}
