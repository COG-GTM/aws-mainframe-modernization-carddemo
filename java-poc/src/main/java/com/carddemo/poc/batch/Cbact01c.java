package com.carddemo.poc.batch;

import com.carddemo.poc.copybook.AccountRecord;
import com.carddemo.poc.io.FileStatus;
import com.carddemo.poc.io.FixedLengthRecordReader;

import java.io.PrintStream;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Java port of the read/display path of {@code app/cbl/CBACT01C.cbl}: read the account
 * master file sequentially and print each field of every record.
 *
 * <p>Paragraph mapping:
 * <ul>
 *   <li>{@code 0000-ACCTFILE-OPEN}      -> {@link #openAcctFile()}</li>
 *   <li>{@code 1000-ACCTFILE-GET-NEXT}  -> {@link #getNextAcctRecord()}</li>
 *   <li>{@code 1100-DISPLAY-ACCT-RECORD} -> {@link #displayAcctRecord()}</li>
 *   <li>{@code 9000-ACCTFILE-CLOSE}     -> {@link #closeAcctFile()}</li>
 * </ul>
 *
 * <p>Out of scope for this POC (see README "Known limitations"): the three output files
 * OUTFILE / ARRYFILE / VBRCFILE written by paragraphs 1300-1575, and the
 * {@code CALL 'COBDATFT'} to the assembler date-formatting routine.
 */
public final class Cbact01c extends BatchProgram {

    public static final String PROGRAM_ID = "CBACT01C";

    private final FixedLengthRecordReader acctFile;

    /** WORKING-STORAGE: ACCOUNT-RECORD (COPY CVACT01Y). */
    private AccountRecord accountRecord;
    private int applResult;
    private boolean endOfFile;

    public Cbact01c(FixedLengthRecordReader acctFile, PrintStream sysout) {
        super(sysout);
        this.acctFile = acctFile;
    }

    public Cbact01c(Path acctFile, PrintStream sysout) {
        this(FixedLengthRecordReader.forDataFile(acctFile, AccountRecord.RECORD_LENGTH), sysout);
    }

    public void run() {
        display("START OF EXECUTION OF PROGRAM " + PROGRAM_ID);
        openAcctFile();

        while (!endOfFile) {
            getNextAcctRecord();
            if (!endOfFile) {
                display(accountRecord.toDisplayString());
            }
        }

        closeAcctFile();
        display("END OF EXECUTION OF PROGRAM " + PROGRAM_ID);
    }

    /** {@code 1000-ACCTFILE-GET-NEXT}. */
    private void getNextAcctRecord() {
        Optional<byte[]> record = acctFile.readNext();
        FileStatus status = acctFile.status();
        if (status == FileStatus.SUCCESS) {
            applResult = APPL_AOK;
            accountRecord = AccountRecord.fromBytes(record.orElseThrow());
            displayAcctRecord();
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
            display("ERROR READING ACCOUNT FILE");
            displayIoStatus(status);
            throw abend("READ ACCTFILE status " + status.code());
        }
    }

    /**
     * {@code 1100-DISPLAY-ACCT-RECORD}. Numeric fields are displayed as their zoned-decimal
     * picture text (e.g. {@code 00000001940{}), exactly as Enterprise COBOL DISPLAY does for
     * {@code PIC S9(10)V99} USAGE DISPLAY items.
     */
    private void displayAcctRecord() {
        AccountRecord r = accountRecord;
        display("ACCT-ID                 :" + r.getAcctIdText());
        display("ACCT-ACTIVE-STATUS      :" + r.getActiveStatus());
        display("ACCT-CURR-BAL           :" + r.getCurrBalText());
        display("ACCT-CREDIT-LIMIT       :" + r.getCreditLimitText());
        display("ACCT-CASH-CREDIT-LIMIT  :" + r.getCashCreditLimitText());
        display("ACCT-OPEN-DATE          :" + r.getOpenDate());
        display("ACCT-EXPIRAION-DATE     :" + r.getExpirationDate());
        display("ACCT-REISSUE-DATE       :" + r.getReissueDate());
        display("ACCT-CURR-CYC-CREDIT    :" + r.getCurrCycCreditText());
        display("ACCT-CURR-CYC-DEBIT     :" + r.getCurrCycDebitText());
        display("ACCT-GROUP-ID           :" + r.getGroupId());
        display("-------------------------------------------------");
    }

    /** {@code 0000-ACCTFILE-OPEN}. */
    private void openAcctFile() {
        applResult = 8;
        FileStatus status = acctFile.open();
        applResult = status == FileStatus.SUCCESS ? APPL_AOK : APPL_ERROR;
        if (applResult != APPL_AOK) {
            display("ERROR OPENING ACCTFILE");
            displayIoStatus(status);
            throw abend("OPEN ACCTFILE status " + status.code());
        }
    }

    /** {@code 9000-ACCTFILE-CLOSE}. */
    private void closeAcctFile() {
        applResult = 8;
        acctFile.close();
        FileStatus status = acctFile.status();
        applResult = status == FileStatus.SUCCESS ? APPL_AOK : APPL_ERROR;
        if (applResult != APPL_AOK) {
            display("ERROR CLOSING ACCOUNT FILE");
            displayIoStatus(status);
            throw abend("CLOSE ACCTFILE status " + status.code());
        }
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("usage: Cbact01c <ACCTFILE path>");
            System.exit(2);
        }
        try {
            new Cbact01c(Path.of(args[0]), System.out).run();
        } catch (AbendException e) {
            System.err.println("ABEND U" + e.abendCode() + ": " + e.getMessage());
            System.exit(ABEND_EXIT_CODE);
        }
    }
}
