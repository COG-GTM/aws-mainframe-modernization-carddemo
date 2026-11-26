package com.carddemo.programs.cbpaup0c.ims;

/**
 * Outcome of a DL/I get call: the DIBSTAT value and, when the call succeeded, the segment that was moved
 * into the I/O area. {@code segment} is null when nothing was returned (the COBOL I/O area is left unchanged).
 */
public record DliResult<T>(String status, T segment) {

    public static <T> DliResult<T> of(String status, T segment) {
        return new DliResult<>(status, segment);
    }

    public static <T> DliResult<T> status(String status) {
        return new DliResult<>(status, null);
    }
}
