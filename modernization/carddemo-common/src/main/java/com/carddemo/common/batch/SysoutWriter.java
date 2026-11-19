package com.carddemo.common.batch;

import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * The step's {@code SYSOUT} (D-007): every COBOL {@code DISPLAY} line goes to stdout and, when the launch binds
 * {@code --dd=SYSOUT=<path>}, is appended to that file (a restart appends to the spool of the failed run, like JES).
 * {@link #abend} reproduces the CardDemo abend sequence: the error text, {@code 9910-DISPLAY-IO-STATUS} and
 * {@code 9999-ABEND-PROGRAM}'s {@code ABENDING PROGRAM}.
 */
public class SysoutWriter {

    public static final String ABENDING_PROGRAM = "ABENDING PROGRAM";

    private final Path file;
    private final PrintStream out;
    private boolean abended;

    public SysoutWriter(Path file, PrintStream out) {
        this.file = file;
        this.out = out;
    }

    public synchronized void println(String line) {
        out.println(line);
        if (file != null) {
            try {
                Files.writeString(file, line + "\n", StandardCharsets.ISO_8859_1, StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND);
            } catch (IOException e) {
                throw new UncheckedIOException("Cannot write SYSOUT " + file, e);
            }
        }
    }

    /**
     * {@code 9910-DISPLAY-IO-STATUS} for a numeric 2-digit FILE STATUS not starting with 9:
     * {@code FILE STATUS IS: NNNN00<status>}.
     */
    public static String fileStatusLine(String status) {
        if (!status.matches("[0-8][0-9]")) {
            throw new IllegalArgumentException("Only numeric FILE STATUS 00-89 is rendered: " + status);
        }
        return "FILE STATUS IS: NNNN00" + status;
    }

    /** Error line + FILE STATUS line + {@code ABENDING PROGRAM}. */
    public synchronized void abend(String message, String fileStatus) {
        println(message);
        println(fileStatusLine(fileStatus));
        abend();
    }

    /** {@code ABENDING PROGRAM} once, for abends that have no specific error line. */
    public synchronized void abend() {
        if (!abended) {
            println(ABENDING_PROGRAM);
            abended = true;
        }
    }

    public synchronized boolean abended() {
        return abended;
    }
}
