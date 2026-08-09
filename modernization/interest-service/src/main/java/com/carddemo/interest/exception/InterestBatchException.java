package com.carddemo.interest.exception;

/**
 * Base type for interest-cycle failures.
 *
 * <p>CBACT04C signals every failure the same way: it moves a two-character {@code FILE STATUS}
 * into {@code IO-STATUS}, displays it in {@code 9910-DISPLAY-IO-STATUS} and calls {@code CEE3ABD}
 * with abend code 999 ({@code app/cbl/CBACT04C.cbl:628-648}). The Java port replaces that single
 * abend path with typed exceptions so callers can distinguish missing reference data from a
 * corrupt record.
 */
public class InterestBatchException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public InterestBatchException(String message) {
        super(message);
    }
}
