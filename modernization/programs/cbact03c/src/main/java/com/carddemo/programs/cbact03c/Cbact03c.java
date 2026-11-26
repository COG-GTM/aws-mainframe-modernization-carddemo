package com.carddemo.programs.cbact03c;

import com.carddemo.programs.cbact03c.copybook.CardXrefRecord;
import com.carddemo.programs.cbact03c.copybook.Pic;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * CBACT03C - Read and print account cross reference data file.
 *
 * <p>Reads XREFFILE (CARDXREF KSDS, layout CVACT03Y) sequentially and DISPLAYs every record.
 * Each successfully read record is DISPLAYed twice: once inside 1000-XREFFILE-GET-NEXT and once in
 * the main loop, exactly as the COBOL does. Any non-"00"/"10" read status, or a non-"00" OPEN/CLOSE
 * status, DISPLAYs the error, the formatted file status and ABENDs with U999.
 */
public class Cbact03c {

    static final int APPL_AOK = 0;
    static final int APPL_EOF = 16;
    static final int ABEND_CODE = 999;

    private final XrefFile xrefFile;
    private final Consumer<String> sysout;
    private final Charset nativeCharset;

    /** CARD-XREF-RECORD (CVACT03Y) working-storage image, filled by READ ... INTO. */
    private String cardXrefRecord = " ".repeat(CardXrefRecord.RECORD_LENGTH);
    /** XREFFILE-STATUS. */
    private String xrefFileStatus = "  ";
    /** IO-STATUS. */
    private String ioStatus = "  ";
    /** APPL-RESULT PIC S9(9) COMP. */
    private int applResult;
    /** END-OF-FILE PIC X(01) VALUE 'N'. */
    private char endOfFile = 'N';

    public Cbact03c(XrefFile xrefFile, Consumer<String> sysout) {
        this(xrefFile, sysout, StandardCharsets.ISO_8859_1);
    }

    /**
     * @param nativeCharset charset used to obtain the byte value of IO-STAT2 in 9910-DISPLAY-IO-STATUS
     *                      (ISO-8859-1 matches an ASCII runtime such as GnuCOBOL; IBM037 matches z/OS).
     */
    public Cbact03c(XrefFile xrefFile, Consumer<String> sysout, Charset nativeCharset) {
        this.xrefFile = Objects.requireNonNull(xrefFile);
        this.sysout = Objects.requireNonNull(sysout);
        this.nativeCharset = Objects.requireNonNull(nativeCharset);
    }

    /** PROCEDURE DIVISION. Returns normally on GOBACK; throws {@link AbendException} on ABEND. */
    public void run() {
        display("START OF EXECUTION OF PROGRAM CBACT03C");
        xrefFileOpen();

        while (endOfFile != 'Y') {
            if (endOfFile == 'N') {
                xrefFileGetNext();
                if (endOfFile == 'N') {
                    display(cardXrefRecord);
                }
            }
        }

        xrefFileClose();

        display("END OF EXECUTION OF PROGRAM CBACT03C");
    }

    // 1000-XREFFILE-GET-NEXT
    void xrefFileGetNext() {
        XrefFile.ReadResult result = xrefFile.readNext();
        xrefFileStatus = result.status();
        if (FileStatus.OK.equals(xrefFileStatus)) {
            cardXrefRecord = result.record();
            applResult = 0;
            display(cardXrefRecord);
        } else if (FileStatus.END_OF_FILE.equals(xrefFileStatus)) {
            applResult = 16;
        } else {
            applResult = 12;
        }
        if (isApplAok()) {
            return;
        }
        if (isApplEof()) {
            endOfFile = 'Y';
        } else {
            display("ERROR READING XREFFILE");
            ioStatus = xrefFileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 0000-XREFFILE-OPEN
    void xrefFileOpen() {
        applResult = 8;
        xrefFileStatus = xrefFile.open();
        applResult = FileStatus.OK.equals(xrefFileStatus) ? 0 : 12;
        if (!isApplAok()) {
            display("ERROR OPENING XREFFILE");
            ioStatus = xrefFileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 9000-XREFFILE-CLOSE
    void xrefFileClose() {
        applResult = 8;
        xrefFileStatus = xrefFile.close();
        if (FileStatus.OK.equals(xrefFileStatus)) {
            applResult -= applResult;
        } else {
            applResult = 12;
        }
        if (!isApplAok()) {
            display("ERROR CLOSING XREFFILE");
            ioStatus = xrefFileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 9999-ABEND-PROGRAM
    void abendProgram() {
        display("ABENDING PROGRAM");
        throw new AbendException(ABEND_CODE, 0);
    }

    // 9910-DISPLAY-IO-STATUS
    void displayIoStatus() {
        display("FILE STATUS IS: NNNN" + formatIoStatus04(ioStatus, nativeCharset));
    }

    /** Builds IO-STATUS-04 (PIC 9 + PIC 999) from a two-byte IO-STATUS. */
    static String formatIoStatus04(String ioStatus, Charset nativeCharset) {
        String status = (ioStatus + "  ").substring(0, 2);
        char stat1 = status.charAt(0);
        char stat2 = status.charAt(1);
        if (!Pic.isNumeric(status) || stat1 == '9') {
            // TWO-BYTES-BINARY PIC 9(4) BINARY with IO-STAT2 in its low-order byte -> PIC 999
            int twoBytesBinary = String.valueOf(stat2).getBytes(nativeCharset)[0] & 0xFF;
            return stat1 + String.format(Locale.ROOT, "%03d", twoBytesBinary % 1000);
        }
        return "00" + status;
    }

    boolean isApplAok() {
        return applResult == APPL_AOK;
    }

    boolean isApplEof() {
        return applResult == APPL_EOF;
    }

    int applResult() {
        return applResult;
    }

    char endOfFile() {
        return endOfFile;
    }

    private void display(String line) {
        sysout.accept(line);
    }
}
