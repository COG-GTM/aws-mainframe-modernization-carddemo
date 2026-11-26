package com.carddemo.programs.copaus0c.gateway;

/** Outcome of an EXEC DLI call: DIBSTAT plus the segment moved INTO the I/O area (null when none). */
public record DliResult<T>(String dibstat, T segment) {

    public static <T> DliResult<T> ok(T segment) {
        return new DliResult<>("  ", segment);
    }

    public static <T> DliResult<T> status(String dibstat) {
        return new DliResult<>(dibstat, null);
    }
}
