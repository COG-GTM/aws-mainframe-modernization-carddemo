package com.carddemo.runtime.batch;

/**
 * Emulates a batch ABEND (e.g. CEE3ABD, or a program that forces an abend). The step ends with the
 * abend code instead of a return code; later steps are skipped unless their COND allows it.
 */
public final class Abend extends RuntimeException {

    private final int abendCode;

    public Abend(int abendCode, String message) {
        super("ABEND U" + String.format("%04d", abendCode) + (message == null ? "" : ": " + message));
        this.abendCode = abendCode;
    }

    public int abendCode() {
        return abendCode;
    }
}
