package com.carddemo.runtime.batch;

/** Completion of a step: a return code, an abend code, or bypassed (FLUSH). */
public record StepResult(String stepName, Status status, int code) {

    public enum Status {
        COMPLETED, ABENDED, BYPASSED
    }
}
