package com.carddemo.batch.harness;

/**
 * Ends a step with a failing condition code chosen by the program ({@code MOVE 8 TO RETURN-CODE} + {@code GOBACK}
 * on an error path), as opposed to an abend.
 */
public class ReturnCodeException extends RuntimeException {

    private final ReturnCode returnCode;

    public ReturnCodeException(ReturnCode returnCode, String message) {
        super(returnCode.label() + ": " + message);
        if (!returnCode.isFailure()) {
            throw new IllegalArgumentException("a step that completes sets " + returnCode.label()
                    + " with ReturnCode.set, it does not throw");
        }
        this.returnCode = returnCode;
    }

    public ReturnCode returnCode() {
        return returnCode;
    }
}
