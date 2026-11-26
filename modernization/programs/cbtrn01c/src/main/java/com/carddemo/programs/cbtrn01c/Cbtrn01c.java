package com.carddemo.programs.cbtrn01c;

import com.carddemo.programs.cbtrn01c.copybook.AccountRecord;
import com.carddemo.programs.cbtrn01c.copybook.CardRecord;
import com.carddemo.programs.cbtrn01c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn01c.copybook.CustomerRecord;
import com.carddemo.programs.cbtrn01c.copybook.DalytranRecord;
import com.carddemo.programs.cbtrn01c.copybook.TranRecord;
import com.carddemo.programs.cbtrn01c.io.FileStatus;
import com.carddemo.programs.cbtrn01c.io.FixedWidthIndexedFile;
import com.carddemo.programs.cbtrn01c.io.FixedWidthSequentialFile;
import com.carddemo.programs.cbtrn01c.io.IndexedInputFile;
import com.carddemo.programs.cbtrn01c.io.ReadResult;
import com.carddemo.programs.cbtrn01c.io.SequentialInputFile;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * CBTRN01C: reads the daily transaction file and verifies each transaction's card number against
 * the card cross-reference file and its account against the account file. Nothing is written;
 * the outcome is the DISPLAY log (SYSOUT). CUSTFILE, CARDFILE and TRANFILE are opened and closed
 * but never read.
 */
public final class Cbtrn01c {

    public static final int ABEND_CODE = 999;
    public static final List<String> DD_NAMES =
            List.of("DALYTRAN", "CUSTFILE", "XREFFILE", "CARDFILE", "ACCTFILE", "TRANFILE");

    private final SequentialInputFile<DalytranRecord> dalytranFile;
    private final IndexedInputFile<CustomerRecord> customerFile;
    private final IndexedInputFile<CardXrefRecord> xrefFile;
    private final IndexedInputFile<CardRecord> cardFile;
    private final IndexedInputFile<AccountRecord> accountFile;
    private final IndexedInputFile<TranRecord> transactFile;
    private final Consumer<String> display;
    private final Charset statusCharset;

    // WORKING-STORAGE
    private DalytranRecord dalytranRecord = DalytranRecord.initial();
    // DALYTRAN-RECORD bytes as read, so DISPLAY echoes them unchanged (e.g. unsigned trailing digit).
    private String dalytranImage = dalytranRecord.toFixedWidth();
    private CardXrefRecord cardXrefRecord = CardXrefRecord.initial();
    private AccountRecord accountRecord = AccountRecord.initial();
    private String dalytranStatus = "  ";
    private String custfileStatus = "  ";
    private String xreffileStatus = "  ";
    private String cardfileStatus = "  ";
    private String acctfileStatus = "  ";
    private String tranfileStatus = "  ";
    private String ioStatus = "  ";
    private final ApplResult applResult = new ApplResult();
    private char endOfDailyTransFile = 'N';
    private int wsXrefReadStatus;
    private int wsAcctReadStatus;
    private boolean started;

    public Cbtrn01c(SequentialInputFile<DalytranRecord> dalytranFile,
                    IndexedInputFile<CustomerRecord> customerFile,
                    IndexedInputFile<CardXrefRecord> xrefFile,
                    IndexedInputFile<CardRecord> cardFile,
                    IndexedInputFile<AccountRecord> accountFile,
                    IndexedInputFile<TranRecord> transactFile,
                    Consumer<String> display) {
        this(dalytranFile, customerFile, xrefFile, cardFile, accountFile, transactFile, display,
                StandardCharsets.ISO_8859_1);
    }

    /**
     * @param statusCharset code page used to take the binary value of FILE STATUS byte 2 in
     *                      Z-DISPLAY-IO-STATUS (IBM-037 reproduces z/OS output, ISO-8859-1 GnuCOBOL).
     */
    public Cbtrn01c(SequentialInputFile<DalytranRecord> dalytranFile,
                    IndexedInputFile<CustomerRecord> customerFile,
                    IndexedInputFile<CardXrefRecord> xrefFile,
                    IndexedInputFile<CardRecord> cardFile,
                    IndexedInputFile<AccountRecord> accountFile,
                    IndexedInputFile<TranRecord> transactFile,
                    Consumer<String> display,
                    Charset statusCharset) {
        this.dalytranFile = Objects.requireNonNull(dalytranFile);
        this.customerFile = Objects.requireNonNull(customerFile);
        this.xrefFile = Objects.requireNonNull(xrefFile);
        this.cardFile = Objects.requireNonNull(cardFile);
        this.accountFile = Objects.requireNonNull(accountFile);
        this.transactFile = Objects.requireNonNull(transactFile);
        this.display = Objects.requireNonNull(display);
        this.statusCharset = Objects.requireNonNull(statusCharset);
    }

    /**
     * MAIN-PARA. Returns RETURN-CODE 0 on GOBACK. An instance is one run unit: it can run once,
     * so create a new instance (and new files) to run again.
     *
     * @throws AbendException         Z-ABEND-PROGRAM (CEE3ABD, code 999)
     * @throws CobolRuntimeException  unhandled keyed-READ I/O error
     */
    public int run() {
        // One run unit per instance: WORKING-STORAGE is not re-initialised.
        if (started) {
            throw new IllegalStateException("CBTRN01C instance has already run");
        }
        started = true;
        display.accept("START OF EXECUTION OF PROGRAM CBTRN01C");
        dalytranOpen0000();
        custfileOpen0100();
        xreffileOpen0200();
        cardfileOpen0300();
        acctfileOpen0400();
        tranfileOpen0500();

        while (endOfDailyTransFile != 'Y') {
            if (endOfDailyTransFile == 'N') {
                dalytranGetNext1000();
                if (endOfDailyTransFile == 'N') {
                    display.accept(dalytranImage);
                }
                // Not guarded by the EOF check above: after EOF the last record (unchanged by the
                // failed READ INTO) is looked up once more, exactly as the COBOL does.
                wsXrefReadStatus = 0;
                cardXrefRecord = cardXrefRecord.withCardNum(dalytranRecord.cardNum());
                lookupXref2000();
                if (wsXrefReadStatus == 0) {
                    wsAcctReadStatus = 0;
                    accountRecord = accountRecord.withAcctId(cardXrefRecord.acctId());
                    readAccount3000();
                    if (wsAcctReadStatus != 0) {
                        display.accept("ACCOUNT " + accountRecord.acctIdDisplay() + " NOT FOUND");
                    }
                } else {
                    display.accept("CARD NUMBER " + dalytranRecord.cardNum()
                            + " COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-" + dalytranRecord.id());
                }
            }
        }

        dalytranClose9000();
        custfileClose9100();
        xreffileClose9200();
        cardfileClose9300();
        acctfileClose9400();
        tranfileClose9500();

        display.accept("END OF EXECUTION OF PROGRAM CBTRN01C");
        return 0;
    }

    // 1000-DALYTRAN-GET-NEXT
    private void dalytranGetNext1000() {
        ReadResult<DalytranRecord> result = dalytranFile.read();
        dalytranStatus = result.status();
        if (FileStatus.OK.equals(dalytranStatus)) {
            dalytranRecord = result.record();
            dalytranImage = result.image() != null ? result.image() : dalytranRecord.toFixedWidth();
            applResult.set(0);
        } else if (FileStatus.END_OF_FILE.equals(dalytranStatus)) {
            applResult.set(16);
        } else {
            applResult.set(12);
        }
        if (applResult.isAok()) {
            return;
        }
        if (applResult.isEof()) {
            endOfDailyTransFile = 'Y';
        } else {
            display.accept("ERROR READING DAILY TRANSACTION FILE");
            ioStatus = dalytranStatus;
            displayIoStatusZ();
            abendProgramZ();
        }
    }

    // 2000-LOOKUP-XREF
    private void lookupXref2000() {
        ReadResult<CardXrefRecord> result = xrefFile.read(cardXrefRecord.cardNum());
        xreffileStatus = result.status();
        if (FileStatus.isInvalidKey(xreffileStatus)) {
            display.accept("INVALID CARD NUMBER FOR XREF");
            wsXrefReadStatus = 4;
        } else if (FileStatus.isSuccessful(xreffileStatus)) {
            cardXrefRecord = result.record();
            display.accept("SUCCESSFUL READ OF XREF");
            display.accept("CARD NUMBER: " + cardXrefRecord.cardNum());
            display.accept("ACCOUNT ID : " + cardXrefRecord.acctIdDisplay());
            display.accept("CUSTOMER ID: " + cardXrefRecord.custIdDisplay());
        } else {
            throw new CobolRuntimeException("XREFFILE", xreffileStatus);
        }
    }

    // 3000-READ-ACCOUNT
    private void readAccount3000() {
        ReadResult<AccountRecord> result = accountFile.read(accountRecord.acctIdDisplay());
        acctfileStatus = result.status();
        if (FileStatus.isInvalidKey(acctfileStatus)) {
            display.accept("INVALID ACCOUNT NUMBER FOUND");
            wsAcctReadStatus = 4;
        } else if (FileStatus.isSuccessful(acctfileStatus)) {
            accountRecord = result.record();
            display.accept("SUCCESSFUL READ OF ACCOUNT FILE");
        } else {
            throw new CobolRuntimeException("ACCTFILE", acctfileStatus);
        }
    }

    // 0000-DALYTRAN-OPEN
    private void dalytranOpen0000() {
        dalytranStatus = dalytranFile.open();
        checkOpen(dalytranStatus, "ERROR OPENING DAILY TRANSACTION FILE");
    }

    // 0100-CUSTFILE-OPEN
    private void custfileOpen0100() {
        custfileStatus = customerFile.open();
        checkOpen(custfileStatus, "ERROR OPENING CUSTOMER FILE");
    }

    // 0200-XREFFILE-OPEN
    private void xreffileOpen0200() {
        xreffileStatus = xrefFile.open();
        checkOpen(xreffileStatus, "ERROR OPENING CROSS REF FILE");
    }

    // 0300-CARDFILE-OPEN
    private void cardfileOpen0300() {
        cardfileStatus = cardFile.open();
        checkOpen(cardfileStatus, "ERROR OPENING CARD FILE");
    }

    // 0400-ACCTFILE-OPEN
    private void acctfileOpen0400() {
        acctfileStatus = accountFile.open();
        checkOpen(acctfileStatus, "ERROR OPENING ACCOUNT FILE");
    }

    // 0500-TRANFILE-OPEN
    private void tranfileOpen0500() {
        tranfileStatus = transactFile.open();
        checkOpen(tranfileStatus, "ERROR OPENING TRANSACTION FILE");
    }

    // 9000-DALYTRAN-CLOSE: the COBOL reports CUSTOMER FILE and CUSTFILE-STATUS here; kept as-is.
    private void dalytranClose9000() {
        dalytranStatus = dalytranFile.close();
        checkClose(dalytranStatus, "ERROR CLOSING CUSTOMER FILE", custfileStatus);
    }

    // 9100-CUSTFILE-CLOSE
    private void custfileClose9100() {
        custfileStatus = customerFile.close();
        checkClose(custfileStatus, "ERROR CLOSING CUSTOMER FILE", custfileStatus);
    }

    // 9200-XREFFILE-CLOSE
    private void xreffileClose9200() {
        xreffileStatus = xrefFile.close();
        checkClose(xreffileStatus, "ERROR CLOSING CROSS REF FILE", xreffileStatus);
    }

    // 9300-CARDFILE-CLOSE
    private void cardfileClose9300() {
        cardfileStatus = cardFile.close();
        checkClose(cardfileStatus, "ERROR CLOSING CARD FILE", cardfileStatus);
    }

    // 9400-ACCTFILE-CLOSE
    private void acctfileClose9400() {
        acctfileStatus = accountFile.close();
        checkClose(acctfileStatus, "ERROR CLOSING ACCOUNT FILE", acctfileStatus);
    }

    // 9500-TRANFILE-CLOSE
    private void tranfileClose9500() {
        tranfileStatus = transactFile.close();
        checkClose(tranfileStatus, "ERROR CLOSING TRANSACTION FILE", tranfileStatus);
    }

    private void checkOpen(String status, String errorMessage) {
        applResult.set(8);
        checkClose(status, errorMessage, status);
    }

    private void checkClose(String status, String errorMessage, String statusToReport) {
        applResult.set(FileStatus.OK.equals(status) ? 0 : 12);
        if (!applResult.isAok()) {
            display.accept(errorMessage);
            ioStatus = statusToReport;
            displayIoStatusZ();
            abendProgramZ();
        }
    }

    // Z-ABEND-PROGRAM
    private void abendProgramZ() {
        display.accept("ABENDING PROGRAM");
        throw new AbendException(ABEND_CODE, 0);
    }

    // Z-DISPLAY-IO-STATUS
    private void displayIoStatusZ() {
        display.accept("FILE STATUS IS: NNNN" + formatIoStatus(ioStatus, statusCharset));
    }

    /** IO-STATUS-04 as built by Z-DISPLAY-IO-STATUS. */
    public static String formatIoStatus(String ioStatus, Charset charset) {
        String status = ioStatus == null ? "  " : (ioStatus + "  ").substring(0, 2);
        char stat1 = status.charAt(0);
        boolean numeric = Character.isDigit(stat1) && Character.isDigit(status.charAt(1))
                && stat1 <= '9' && status.charAt(1) <= '9';
        if (!numeric || stat1 == '9') {
            int binary = status.substring(1, 2).getBytes(charset)[0] & 0xFF;
            return stat1 + String.format(Locale.ROOT, "%03d", binary % 1000);
        }
        return "00" + status;
    }

    /**
     * Batch entry point. Each DD is bound by an argument {@code DDNAME=path} or, failing that, an
     * environment variable named after the DD. Exit status 0 on success; on abend U999 the JVM is
     * asked to exit with 999, which POSIX shells observe as 231 (999 mod 256). A SYSOUT write
     * failure also ends with the abend status.
     */
    public static void main(String[] args) {
        System.exit(cli(args, System.getenv(), new FileOutputStream(FileDescriptor.out)));
    }

    /** SYSOUT is written in ISO-8859-1 so each record byte stays one output byte. */
    static int cli(String[] args, Map<String, String> env, OutputStream stdout) {
        Map<String, String> dd = new HashMap<>();
        for (String name : DD_NAMES) {
            String value = env.get(name);
            if (value != null) {
                dd.put(name, value);
            }
        }
        for (String arg : args) {
            int eq = arg.indexOf('=');
            if (eq > 0) {
                dd.put(arg.substring(0, eq).toUpperCase(), arg.substring(eq + 1));
            }
        }
        PrintStream sysout = new PrintStream(stdout, false, StandardCharsets.ISO_8859_1);
        int rc = execute(dd, sysout::println);
        sysout.flush();
        // PrintStream reports an InterruptedIOException only through the thread's interrupt flag.
        boolean sysoutFailed = sysout.checkError() || Thread.currentThread().isInterrupted();
        return rc == 0 && sysoutFailed ? ABEND_CODE : rc;
    }

    static int execute(Map<String, String> dd, Consumer<String> display) {
        var dalytran = new FixedWidthSequentialFile<>(path(dd, "DALYTRAN"), DalytranRecord.CODEC);
        var cust = new FixedWidthIndexedFile<>(path(dd, "CUSTFILE"), CustomerRecord.CODEC, 9);
        var xref = new FixedWidthIndexedFile<>(path(dd, "XREFFILE"), CardXrefRecord.CODEC, 16);
        var card = new FixedWidthIndexedFile<>(path(dd, "CARDFILE"), CardRecord.CODEC, 16);
        var acct = new FixedWidthIndexedFile<>(path(dd, "ACCTFILE"), AccountRecord.CODEC, 11);
        var tran = new FixedWidthIndexedFile<>(path(dd, "TRANFILE"), TranRecord.CODEC, 16);
        Cbtrn01c program = new Cbtrn01c(dalytran, cust, xref, card, acct, tran, display);
        try {
            return program.run();
        } catch (AbendException e) {
            // Abnormal termination closes any files still open; statuses are not reported.
            dalytran.close();
            cust.close();
            xref.close();
            card.close();
            acct.close();
            tran.close();
            return e.abendCode();
        }
    }

    private static Path path(Map<String, String> dd, String name) {
        return Path.of(dd.getOrDefault(name, name));
    }
}
