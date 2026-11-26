package com.carddemo.programs.coadm01c;

/** CICS PGMIDERR condition: the XCTL target program is not defined/installed. */
public class PgmIdErrException extends Exception {

    private final String program;

    public PgmIdErrException(String program) {
        super("PGMIDERR: " + program);
        this.program = program;
    }

    public String program() {
        return program;
    }
}
