package com.carddemo.runtime.batch;

/** One JCL EXEC step: runs a program with its DD bindings and returns the step return code. */
public interface JobStep {

    String stepName();

    /** Returns the RETURN-CODE; throw {@link Abend} for an abnormal end. */
    int run(DdBindings dd);

    /**
     * JCL COND on this step: true when the step must be bypassed given the highest prior return code.
     * Default: run unconditionally after normal completions.
     */
    default boolean bypass(int highestPriorReturnCode) {
        return false;
    }
}
