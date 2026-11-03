package com.carddemo.common.error;

/**
 * A rule that the COBOL programs enforced by sending an error message back to the 3270 screen, for
 * example "Account is not active" or "Overlimit transaction".
 */
public class BusinessRuleException extends RuntimeException {

    public BusinessRuleException(String message) {
        super(message);
    }
}
