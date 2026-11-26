package com.carddemo.programs.cbcus01c;

import com.carddemo.programs.cbcus01c.copybook.CustomerRecord;
import java.nio.charset.Charset;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * CBCUS01C - CardDemo batch program: read and print the customer data file (JCL READCUST, DD
 * CUSTFILE = AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS, output to SYSOUT).
 *
 * <p>Each COBOL paragraph is a method. DISPLAY statements go to the {@code sysout} sink, one line
 * per DISPLAY. CEE3ABD is raised as {@link AbendException} with ABCODE 999.
 */
public final class Cbcus01c {

    public static final String PROGRAM_ID = "CBCUS01C";
    public static final int ABEND_CODE = 999;

    /** Platform charset for Z-DISPLAY-IO-STATUS (binary value of IO-STAT2). Mainframe = EBCDIC. */
    public static final Charset EBCDIC = Charset.forName("IBM037");

    private final CustomerFile custfileFile;
    private final Consumer<String> sysout;
    private final Charset statusCharset;

    // WORKING-STORAGE
    private String customerRecord = CustomerRecord.pic("", CustomerRecord.RECORD_LENGTH);
    private String custfileStatus = "  ";
    private String ioStatus = "  ";
    private String ioStatus04 = "0000";
    private int applResult;
    private char endOfFile = 'N';
    private int abcode;
    private int timing;

    public Cbcus01c(CustomerFile custfileFile, Consumer<String> sysout) {
        this(custfileFile, sysout, EBCDIC);
    }

    public Cbcus01c(CustomerFile custfileFile, Consumer<String> sysout, Charset statusCharset) {
        this.custfileFile = Objects.requireNonNull(custfileFile);
        this.sysout = Objects.requireNonNull(sysout);
        this.statusCharset = Objects.requireNonNull(statusCharset);
    }

    /** PROCEDURE DIVISION. Returns RETURN-CODE 0 on GOBACK; throws {@link AbendException} on abend. */
    public int run() {
        display("START OF EXECUTION OF PROGRAM " + PROGRAM_ID);
        custfileOpen();

        while (endOfFile != 'Y') {
            if (endOfFile == 'N') {
                custfileGetNext();
                if (endOfFile == 'N') {
                    display(customerRecord);
                }
            }
        }

        custfileClose();

        display("END OF EXECUTION OF PROGRAM " + PROGRAM_ID);
        return 0;
    }

    // 1000-CUSTFILE-GET-NEXT
    void custfileGetNext() {
        CustomerFile.ReadResult result = custfileFile.readNext();
        custfileStatus = result.status();
        if (FileStatus.SUCCESS.equals(custfileStatus)) {
            // READ ... INTO CUSTOMER-RECORD: group move of the 500-byte FD record
            customerRecord = CustomerRecord.pic(result.record().toFixedWidth(), CustomerRecord.RECORD_LENGTH);
        }
        if (FileStatus.SUCCESS.equals(custfileStatus)) {
            applResult = 0;
            display(customerRecord);
        } else if (FileStatus.END_OF_FILE.equals(custfileStatus)) {
            applResult = 16;
        } else {
            applResult = 12;
        }
        if (applAok()) {
            // CONTINUE
        } else if (applEof()) {
            endOfFile = 'Y';
        } else {
            display("ERROR READING CUSTOMER FILE");
            ioStatus = custfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 0000-CUSTFILE-OPEN
    void custfileOpen() {
        applResult = 8;
        custfileStatus = custfileFile.open();
        applResult = FileStatus.SUCCESS.equals(custfileStatus) ? 0 : 12;
        if (!applAok()) {
            display("ERROR OPENING CUSTFILE");
            ioStatus = custfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // 9000-CUSTFILE-CLOSE
    void custfileClose() {
        applResult = 8;
        custfileStatus = custfileFile.close();
        applResult = FileStatus.SUCCESS.equals(custfileStatus) ? 0 : 12;
        if (!applAok()) {
            display("ERROR CLOSING CUSTOMER FILE");
            ioStatus = custfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // Z-ABEND-PROGRAM
    void abendProgram() {
        display("ABENDING PROGRAM");
        timing = 0;
        abcode = ABEND_CODE;
        throw new AbendException(abcode, timing);
    }

    // Z-DISPLAY-IO-STATUS
    void displayIoStatus() {
        String status = CustomerRecord.pic(ioStatus, 2);
        char ioStat1 = status.charAt(0);
        char ioStat2 = status.charAt(1);
        if (!isNumeric(status) || ioStat1 == '9') {
            // TWO-BYTES-BINARY PIC 9(4) BINARY with IO-STAT2 in the low-order byte
            int twoBytesBinary = String.valueOf(ioStat2).getBytes(statusCharset)[0] & 0xFF;
            String ioStatus0403 = String.format("%03d", twoBytesBinary % 1000);
            ioStatus04 = ioStat1 + ioStatus0403;
        } else {
            ioStatus04 = "00" + status;
        }
        display("FILE STATUS IS: NNNN" + ioStatus04);
    }

    // 88 APPL-AOK VALUE 0
    boolean applAok() {
        return applResult == 0;
    }

    // 88 APPL-EOF VALUE 16
    boolean applEof() {
        return applResult == 16;
    }

    int applResult() {
        return applResult;
    }

    /** Typed view of the last record read into CUSTOMER-RECORD (CVCUS01Y). */
    public CustomerRecord customerRecord() {
        return CustomerRecord.fromFixedWidth(customerRecord);
    }

    private void display(String line) {
        sysout.accept(line);
    }

    private static boolean isNumeric(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                return false;
            }
        }
        return true;
    }
}
