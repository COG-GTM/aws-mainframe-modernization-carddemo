package com.carddemo.programs.cbact02c;

import com.carddemo.programs.cbact02c.copybook.CardRecord;
import com.carddemo.programs.cbact02c.copybook.Pic;
import com.carddemo.programs.cbact02c.io.CardFile;

import java.util.Objects;
import java.util.function.Consumer;

/**
 * CBACT02C - read and print card data file (batch, JCL READCARD STEP05).
 * Reads CARDFILE sequentially and DISPLAYs every CARD-RECORD (CVACT02Y) to SYSOUT.
 */
public class Cbact02c {

    public static final String PROGRAM_ID = "CBACT02C";
    public static final int ABEND_CODE = 999;

    static final int APPL_RESULT_AOK = 0;
    static final int APPL_RESULT_EOF = 16;

    private final CardFile cardfileFile;
    private final Consumer<String> sysout;

    private String cardRecord = Pic.alpha("", CardRecord.RECORD_LENGTH);
    private String cardfileStatus = "  ";
    private String ioStatus = "  ";
    private int applResult;
    private char endOfFile = 'N';

    public Cbact02c(CardFile cardfileFile, Consumer<String> sysout) {
        this.cardfileFile = Objects.requireNonNull(cardfileFile);
        this.sysout = Objects.requireNonNull(sysout);
    }

    /** PROCEDURE DIVISION. Returns RETURN-CODE 0 on GOBACK; throws {@link AbendException} on abend. */
    public int run() {
        display("START OF EXECUTION OF PROGRAM CBACT02C");
        cardfileOpen();

        while (endOfFile != 'Y') {
            if (endOfFile == 'N') {
                cardfileGetNext();
                if (endOfFile == 'N') {
                    display(cardRecord);
                }
            }
        }

        cardfileClose();

        display("END OF EXECUTION OF PROGRAM CBACT02C");
        return 0;
    }

    // 1000-CARDFILE-GET-NEXT
    void cardfileGetNext() {
        CardFile.ReadResult result = cardfileFile.read();
        cardfileStatus = Pic.alpha(result.status(), 2);
        if (result.record() != null && !"10".equals(cardfileStatus)) {
            // READ ... INTO: group MOVE of FD-CARDFILE-REC into CARD-RECORD
            cardRecord = Pic.alpha(result.record(), CardRecord.RECORD_LENGTH);
        }
        if ("00".equals(cardfileStatus)) {
            applResult = 0;
        } else if ("10".equals(cardfileStatus)) {
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
            display("ERROR READING CARDFILE");
            ioStatus = cardfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 0000-CARDFILE-OPEN
    void cardfileOpen() {
        applResult = 8;
        cardfileStatus = Pic.alpha(cardfileFile.open(), 2);
        if ("00".equals(cardfileStatus)) {
            applResult = 0;
        } else {
            applResult = 12;
        }
        if (!isApplAok()) {
            display("ERROR OPENING CARDFILE");
            ioStatus = cardfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 9000-CARDFILE-CLOSE
    void cardfileClose() {
        applResult = 8;
        cardfileStatus = Pic.alpha(cardfileFile.close(), 2);
        if ("00".equals(cardfileStatus)) {
            applResult -= applResult;
        } else {
            applResult = 12;
        }
        if (!isApplAok()) {
            display("ERROR CLOSING CARDFILE");
            ioStatus = cardfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 9999-ABEND-PROGRAM
    void abendProgram() {
        display("ABENDING PROGRAM");
        int timing = 0;
        int abcode = ABEND_CODE;
        throw new AbendException(abcode, timing);
    }

    // 9910-DISPLAY-IO-STATUS
    void displayIoStatus() {
        display("FILE STATUS IS: NNNN" + formatIoStatus04(ioStatus));
    }

    /**
     * Builds IO-STATUS-04 (PIC 9 + PIC 999) from IO-STATUS. For non-numeric or 9x statuses the
     * second byte is taken as a binary value via TWO-BYTES-BINARY / TWO-BYTES-RIGHT.
     */
    static String formatIoStatus04(String ioStatus) {
        String status = Pic.alpha(ioStatus, 2);
        char ioStat1 = status.charAt(0);
        char ioStat2 = status.charAt(1);
        if (!Pic.isNumeric(status) || ioStat1 == '9') {
            int twoBytesBinary = ioStat2 & 0xFF;
            return ioStat1 + Pic.unsignedNumeric(twoBytesBinary % 1000, 3);
        }
        return "00" + status;
    }

    boolean isApplAok() {
        return applResult == APPL_RESULT_AOK;
    }

    boolean isApplEof() {
        return applResult == APPL_RESULT_EOF;
    }

    int applResult() {
        return applResult;
    }

    String cardfileStatus() {
        return cardfileStatus;
    }

    boolean endOfFile() {
        return endOfFile == 'Y';
    }

    /** CARD-RECORD as last read, decoded per CVACT02Y. */
    public CardRecord currentCardRecord() {
        return CardRecord.fromFixedWidth(cardRecord);
    }

    private void display(String line) {
        sysout.accept(line);
    }
}
