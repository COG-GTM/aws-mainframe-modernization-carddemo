package com.carddemo.runtime.cics;

/**
 * Thrown by the non-returning control commands (RETURN, XCTL, ABEND) so that, as in COBOL, no statement
 * after them executes. Caught by {@link CicsDispatcher}. Programs must not catch it.
 */
public final class CicsControlTransfer extends RuntimeException {

    private final transient CicsOutcome outcome;

    CicsControlTransfer(CicsOutcome outcome) {
        super(outcome.type().name(), null, false, false);
        this.outcome = outcome;
    }

    public CicsOutcome outcome() {
        return outcome;
    }
}
