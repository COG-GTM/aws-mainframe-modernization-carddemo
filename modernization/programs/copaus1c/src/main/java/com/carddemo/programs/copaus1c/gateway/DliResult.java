package com.carddemo.programs.copaus1c.gateway;

/** Outcome of an EXEC DLI call: DIBSTAT plus the segment image moved INTO the I/O area (null when none). */
public record DliResult<T>(String dibstat, T segment) {

    public static <T> DliResult<T> of(String dibstat) {
        return new DliResult<>(dibstat, null);
    }
}
