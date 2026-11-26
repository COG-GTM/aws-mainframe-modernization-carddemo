package com.carddemo.programs.dbunldgs;

/** Raised by 9999-ABEND: the program stops with RETURN-CODE 16. */
public class DbunldgsAbendException extends RuntimeException {
    public DbunldgsAbendException(String message) {
        super(message);
    }
}
