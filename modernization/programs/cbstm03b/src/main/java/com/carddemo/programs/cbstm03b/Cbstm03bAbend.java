package com.carddemo.programs.cbstm03b;

/** Runtime condition that would abend the COBOL run unit (e.g. EC-BOUND-REF-MOD). */
public class Cbstm03bAbend extends RuntimeException {

    public Cbstm03bAbend(String message) {
        super(message);
    }
}
