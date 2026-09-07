package com.carddemo.poc.batch;

import com.carddemo.poc.io.FileStatus;

import java.io.PrintStream;

/**
 * Behaviour common to the CardDemo batch programs: {@code DISPLAY} to SYSOUT, the
 * {@code 9910-DISPLAY-IO-STATUS} formatter and {@code 9999-ABEND-PROGRAM}.
 */
abstract class BatchProgram {

    /** Return code the COBOL program leaves in APPL-RESULT for a normal read. */
    static final int APPL_AOK = 0;
    /** APPL-RESULT value used for end-of-file. */
    static final int APPL_EOF = 16;
    /** APPL-RESULT value used for any other failure. */
    static final int APPL_ERROR = 12;
    /** Process exit status used by main() when the program abends. */
    static final int ABEND_EXIT_CODE = 12;

    protected final PrintStream sysout;

    protected BatchProgram(PrintStream sysout) {
        this.sysout = sysout;
    }

    /** {@code DISPLAY text}. COBOL DISPLAY writes one line per statement. */
    protected void display(String text) {
        sysout.println(text);
    }

    /**
     * {@code 9910-DISPLAY-IO-STATUS}.
     * <p>For numeric statuses other than 9x the output is {@code FILE STATUS IS: NNNN00ss}.
     * For a 9x status the second byte is a binary value and is printed as {@code 9nnn}.
     */
    protected void displayIoStatus(FileStatus status) {
        String io = status.code();
        char stat1 = io.charAt(0);
        char stat2 = io.charAt(1);
        String ioStatus04;
        if (!Character.isDigit(stat1) || !Character.isDigit(stat2) || stat1 == '9') {
            ioStatus04 = stat1 + String.format("%03d", (int) stat2);
        } else {
            ioStatus04 = "00" + io;
        }
        display("FILE STATUS IS: NNNN" + ioStatus04);
    }

    /** {@code 9999-ABEND-PROGRAM}. */
    protected AbendException abend(String context) {
        display("ABENDING PROGRAM");
        return new AbendException(context);
    }
}
