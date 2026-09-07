package com.carddemo.poc.batch;

import com.carddemo.poc.copybook.CardXrefRecord;
import com.carddemo.poc.io.FileStatus;
import com.carddemo.poc.io.FixedLengthRecordReader;

import java.io.PrintStream;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Java port of {@code app/cbl/CBACT03C.cbl}: read and print the account cross-reference file.
 *
 * <p>Paragraph mapping:
 * <ul>
 *   <li>PROCEDURE DIVISION main line -> {@link #run()}</li>
 *   <li>{@code 0000-XREFFILE-OPEN}   -> {@link #openXrefFile()}</li>
 *   <li>{@code 1000-XREFFILE-GET-NEXT} -> {@link #getNextXrefRecord()}</li>
 *   <li>{@code 9000-XREFFILE-CLOSE}  -> {@link #closeXrefFile()}</li>
 *   <li>{@code 9910-DISPLAY-IO-STATUS} / {@code 9999-ABEND-PROGRAM} -> {@link BatchProgram}</li>
 * </ul>
 *
 * <p>Note: the original program DISPLAYs each record twice - once inside
 * {@code 1000-XREFFILE-GET-NEXT} and once in the main loop. That behaviour is reproduced
 * faithfully so the output can be diffed against the mainframe SYSOUT.
 */
public final class Cbact03c extends BatchProgram {

    public static final String PROGRAM_ID = "CBACT03C";

    private final FixedLengthRecordReader xrefFile;

    /** WORKING-STORAGE: CARD-XREF-RECORD (COPY CVACT03Y). */
    private CardXrefRecord cardXrefRecord;
    /** WORKING-STORAGE: APPL-RESULT PIC S9(9) COMP. */
    private int applResult;
    /** WORKING-STORAGE: END-OF-FILE PIC X VALUE 'N'. */
    private boolean endOfFile;

    public Cbact03c(FixedLengthRecordReader xrefFile, PrintStream sysout) {
        super(sysout);
        this.xrefFile = xrefFile;
    }

    /** Convenience constructor: the DD XREFFILE points at a CardDemo sample data file. */
    public Cbact03c(Path xrefFile, PrintStream sysout) {
        this(FixedLengthRecordReader.forDataFile(xrefFile, CardXrefRecord.RECORD_LENGTH), sysout);
    }

    /** Main line of the PROCEDURE DIVISION. */
    public void run() {
        display("START OF EXECUTION OF PROGRAM " + PROGRAM_ID);
        openXrefFile();

        while (!endOfFile) {
            getNextXrefRecord();
            if (!endOfFile) {
                display(cardXrefRecord.toDisplayString());
            }
        }

        closeXrefFile();
        display("END OF EXECUTION OF PROGRAM " + PROGRAM_ID);
    }

    /** {@code 1000-XREFFILE-GET-NEXT}. */
    private void getNextXrefRecord() {
        Optional<byte[]> record = xrefFile.readNext();
        FileStatus status = xrefFile.status();
        if (status == FileStatus.SUCCESS) {
            applResult = APPL_AOK;
            cardXrefRecord = CardXrefRecord.fromBytes(record.orElseThrow());
            display(cardXrefRecord.toDisplayString());
        } else if (status == FileStatus.END_OF_FILE) {
            applResult = APPL_EOF;
        } else {
            applResult = APPL_ERROR;
        }

        if (applResult == APPL_AOK) {
            return;
        }
        if (applResult == APPL_EOF) {
            endOfFile = true;
        } else {
            display("ERROR READING XREFFILE");
            displayIoStatus(status);
            throw abend("READ XREFFILE status " + status.code());
        }
    }

    /** {@code 0000-XREFFILE-OPEN}. */
    private void openXrefFile() {
        applResult = 8;
        FileStatus status = xrefFile.open();
        applResult = status == FileStatus.SUCCESS ? APPL_AOK : APPL_ERROR;
        if (applResult != APPL_AOK) {
            display("ERROR OPENING XREFFILE");
            displayIoStatus(status);
            throw abend("OPEN XREFFILE status " + status.code());
        }
    }

    /** {@code 9000-XREFFILE-CLOSE}. */
    private void closeXrefFile() {
        applResult = 8;
        xrefFile.close();
        FileStatus status = xrefFile.status();
        applResult = status == FileStatus.SUCCESS ? APPL_AOK : APPL_ERROR;
        if (applResult != APPL_AOK) {
            display("ERROR CLOSING XREFFILE");
            displayIoStatus(status);
            throw abend("CLOSE XREFFILE status " + status.code());
        }
    }

    /**
     * JCL equivalent: {@code //XREFFILE DD DSN=...}. Pass the data file path as the only argument.
     * Exits with 12 on abend (the COBOL APPL-RESULT error value; ABCODE 999 does not fit in a
     * process exit status).
     */
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("usage: Cbact03c <XREFFILE path>");
            System.exit(2);
        }
        try {
            new Cbact03c(Path.of(args[0]), System.out).run();
        } catch (AbendException e) {
            System.err.println("ABEND U" + e.abendCode() + ": " + e.getMessage());
            System.exit(ABEND_EXIT_CODE);
        }
    }
}
