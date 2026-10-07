package com.carddemo.common;

import java.util.Objects;

/**
 * {@code DFHRESP(DUPREC)} / {@code DFHRESP(DUPKEY)} / file status 22: the key already exists (HTTP 409, ADR-0009).
 * The condition is kept so the response reports the one the COBOL program would have seen.
 */
public class DuplicateRecordException extends RuntimeException {

    /** The CICS condition: {@code DUPREC} (primary key) or {@code DUPKEY} (alternate index key). */
    public enum Condition { DUPREC, DUPKEY }

    private final Condition condition;

    public DuplicateRecordException(String message) {
        this(Condition.DUPREC, message);
    }

    public DuplicateRecordException(Condition condition, String message) {
        super(message);
        this.condition = Objects.requireNonNull(condition, "condition");
    }

    public static DuplicateRecordException duplicateKey(String message) {
        return new DuplicateRecordException(Condition.DUPKEY, message);
    }

    public Condition condition() {
        return condition;
    }
}
