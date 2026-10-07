package com.carddemo.batch.program;

import com.carddemo.batch.io.AbendException;
import com.carddemo.batch.io.FileStatusException;
import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.io.SequentialFile;
import com.carddemo.batch.record.AccountRecord;
import com.carddemo.batch.record.CardRecord;
import com.carddemo.batch.record.CardXrefRecord;
import com.carddemo.batch.record.CustomerRecord;
import com.carddemo.batch.record.DalytranRecord;
import com.carddemo.batch.record.TranRecord;

import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Java 17 port of {@code app/cbl/CBTRN01C.cbl}: read the daily transaction file sequentially and, for each
 * record, verify the card number against XREFFILE and the cross-referenced account against ACCTFILE,
 * DISPLAYing the same lines as the COBOL program. CUSTFILE, CARDFILE and TRANFILE are opened and closed
 * (as the COBOL does) but never read.
 *
 * <p>The paragraph structure of the COBOL program is kept one-to-one (method names carry the paragraph
 * names); WORKING-STORAGE items are instance fields; the copybook record areas are the record classes,
 * reused across records exactly as COBOL does. In addition to the DISPLAY log, the program collects one
 * {@link TransactionOutcome} per DALYTRAN record, in file order.
 */
public final class Cbtrn01c {

    public static final int ABEND_CODE = 999;
    private static final int APPL_AOK = 0;
    private static final int APPL_EOF = 16;
    private static final int READ_FAILED = 4;

    // ----- FILE SECTION -----------------------------------------------------------------------
    private final SequentialFile dalytranFile;
    private final KsdsFile customerFile;
    private final KsdsFile xrefFile;
    private final KsdsFile cardFile;
    private final KsdsFile accountFile;
    private final KsdsFile transactFile;

    // ----- WORKING-STORAGE SECTION ------------------------------------------------------------
    private final DalytranRecord dalytranRecord = new DalytranRecord();
    private String dalytranStatus = "00";
    private final CustomerRecord customerRecord = new CustomerRecord();
    private String custfileStatus = "00";
    private final CardXrefRecord cardXrefRecord = new CardXrefRecord();
    private String xreffileStatus = "00";
    private final CardRecord cardRecord = new CardRecord();
    private String cardfileStatus = "00";
    private final AccountRecord accountRecord = new AccountRecord();
    private String acctfileStatus = "00";
    private final TranRecord tranRecord = new TranRecord();
    private String tranfileStatus = "00";
    private String ioStatus = "00";
    private int applResult;
    private boolean endOfDailyTransFile;
    private int wsXrefReadStatus;
    private int wsAcctReadStatus;

    private final List<TransactionOutcome> outcomes = new ArrayList<>();
    private final PrintStream display;

    /**
     * @param dalytran the DALYTRAN sequential file (350-byte records, optionally one per line)
     * @param custfile CUSTFILE KSDS, {@code load} decides how its text is split into 500-byte records
     * @param xreffile XREFFILE KSDS (50-byte records keyed by XREF-CARD-NUM)
     * @param cardfile CARDFILE KSDS (150-byte records keyed by CARD-NUM)
     * @param acctfile ACCTFILE KSDS (300-byte records keyed by ACCT-ID)
     * @param tranfile TRANFILE KSDS (350-byte records keyed by TRAN-ID); may be empty
     * @param load     how the five KSDS text files are split into records; {@link KsdsFile.LoadFormat#LINE_SEQUENTIAL}
     *                 pads short lines the way the mainframe REPRO / harness KSDSLOAD do
     */
    public Cbtrn01c(Path dalytran, Path custfile, Path xreffile, Path cardfile, Path acctfile, Path tranfile,
                    KsdsFile.LoadFormat load, PrintStream display) {
        this.dalytranFile = new SequentialFile("DALYTRAN", dalytran, DalytranRecord.LENGTH);
        this.customerFile = new KsdsFile("CUSTFILE", custfile, CustomerRecord.LENGTH,
                CustomerRecord.CUST_ID.offset(), CustomerRecord.CUST_ID.length(), load);
        this.xrefFile = new KsdsFile("XREFFILE", xreffile, CardXrefRecord.LENGTH,
                CardXrefRecord.XREF_CARD_NUM.offset(), CardXrefRecord.XREF_CARD_NUM.length(), load);
        this.cardFile = new KsdsFile("CARDFILE", cardfile, CardRecord.LENGTH,
                CardRecord.CARD_NUM.offset(), CardRecord.CARD_NUM.length(), load);
        this.accountFile = new KsdsFile("ACCTFILE", acctfile, AccountRecord.LENGTH,
                AccountRecord.ACCT_ID.offset(), AccountRecord.ACCT_ID.length(), load);
        this.transactFile = new KsdsFile("TRANFILE", tranfile, TranRecord.LENGTH,
                TranRecord.TRAN_ID.offset(), TranRecord.TRAN_ID.length(), load);
        this.display = display;
    }

    public Cbtrn01c(Path dalytran, Path custfile, Path xreffile, Path cardfile, Path acctfile, Path tranfile,
                    PrintStream display) {
        this(dalytran, custfile, xreffile, cardfile, acctfile, tranfile, KsdsFile.LoadFormat.LINE_SEQUENTIAL, display);
    }

    /**
     * {@code java ... Cbtrn01c [DALYTRAN [CUSTFILE [XREFFILE [CARDFILE [ACCTFILE [TRANFILE]]]]]]}. The inputs
     * default to the {@code app/data/ASCII} sample files; TRANFILE defaults to a file named {@code TRANFILE} in
     * the working directory (the harness supplies an empty one, as the JCL's TRANSACT cluster holds no sample
     * data). Exits with the abend code (999) on a CEE3ABD.
     */
    public static void main(String[] args) {
        Path data = Paths.get("app", "data", "ASCII");
        Path dalytran = args.length > 0 ? Paths.get(args[0]) : data.resolve("dailytran.txt");
        Path custfile = args.length > 1 ? Paths.get(args[1]) : data.resolve("custdata.txt");
        Path xreffile = args.length > 2 ? Paths.get(args[2]) : data.resolve("cardxref.txt");
        Path cardfile = args.length > 3 ? Paths.get(args[3]) : data.resolve("carddata.txt");
        Path acctfile = args.length > 4 ? Paths.get(args[4]) : data.resolve("acctdata.txt");
        Path tranfile = args.length > 5 ? Paths.get(args[5]) : Paths.get("TRANFILE");
        try {
            new Cbtrn01c(dalytran, custfile, xreffile, cardfile, acctfile, tranfile, System.out).run();
        } catch (AbendException e) {
            System.out.println(e.getMessage());
            System.out.flush();
            System.exit(e.abendCode());
        }
        System.out.flush();
    }

    // ----- MAIN-PARA --------------------------------------------------------------------------
    /** PROCEDURE DIVISION. Returns the COBOL RETURN-CODE (0); abends surface as {@link AbendException}. */
    public int run() {
        display.println("START OF EXECUTION OF PROGRAM CBTRN01C");
        dalytranOpen();            // 0000-DALYTRAN-OPEN
        custfileOpen();            // 0100-CUSTFILE-OPEN
        xreffileOpen();            // 0200-XREFFILE-OPEN
        cardfileOpen();            // 0300-CARDFILE-OPEN
        acctfileOpen();            // 0400-ACCTFILE-OPEN
        tranfileOpen();            // 0500-TRANFILE-OPEN

        while (!endOfDailyTransFile) {
            dalytranGetNext();     // 1000-DALYTRAN-GET-NEXT
            if (!endOfDailyTransFile) {
                display.println(dalytranRecord.display());
            }
            // The COBOL performs the lookups once more after the AT END read, against the record still in
            // DALYTRAN-RECORD (READ ... INTO leaves it unchanged at end of file); that extra lookup is
            // DISPLAYed exactly as the COBOL does but is not a transaction, so it produces no outcome row.
            wsXrefReadStatus = 0;
            cardXrefRecord.setXrefCardNum(dalytranRecord.dalytranCardNum());
            lookupXref();          // 2000-LOOKUP-XREF
            if (wsXrefReadStatus == 0) {
                wsAcctReadStatus = 0;
                accountRecord.setAcctId(cardXrefRecord.xrefAcctId());
                readAccount();     // 3000-READ-ACCOUNT
                if (wsAcctReadStatus != 0) {
                    display.println("ACCOUNT " + accountRecord.display(AccountRecord.ACCT_ID) + " NOT FOUND");
                }
                if (!endOfDailyTransFile) {
                    outcomes.add(TransactionOutcome.accountLookedUp(dalytranRecord.dalytranId(),
                            dalytranRecord.dalytranCardNum(), accountRecord.acctId(), wsAcctReadStatus == 0));
                }
            } else {
                display.println("CARD NUMBER " + dalytranRecord.dalytranCardNum()
                        + " COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-" + dalytranRecord.dalytranId());
                if (!endOfDailyTransFile) {
                    outcomes.add(TransactionOutcome.cardNotFound(dalytranRecord.dalytranId(),
                            dalytranRecord.dalytranCardNum()));
                }
            }
        }

        dalytranClose();           // 9000-DALYTRAN-CLOSE
        custfileClose();           // 9100-CUSTFILE-CLOSE
        xreffileClose();           // 9200-XREFFILE-CLOSE
        cardfileClose();           // 9300-CARDFILE-CLOSE
        acctfileClose();           // 9400-ACCTFILE-CLOSE
        tranfileClose();           // 9500-TRANFILE-CLOSE

        display.println("END OF EXECUTION OF PROGRAM CBTRN01C");
        display.flush();
        return 0;
    }

    // ----- 1000-DALYTRAN-GET-NEXT -------------------------------------------------------------
    void dalytranGetNext() {
        Optional<byte[]> next;
        try {
            next = dalytranFile.readNext();
            dalytranStatus = next.isPresent() ? "00" : FileStatusException.END_OF_FILE;
        } catch (FileStatusException e) {
            dalytranStatus = e.status();
            next = Optional.empty();
        }
        if ("00".equals(dalytranStatus)) {
            dalytranRecord.moveFrom(next.get());
            applResult = 0;
        } else if (FileStatusException.END_OF_FILE.equals(dalytranStatus)) {
            applResult = APPL_EOF;
        } else {
            applResult = 12;
        }
        if (applResult != APPL_AOK) {
            if (applResult == APPL_EOF) {
                endOfDailyTransFile = true;
            } else {
                display.println("ERROR READING DAILY TRANSACTION FILE");
                ioStatus = dalytranStatus;
                displayIoStatus();
                abendProgram();
            }
        }
    }

    // ----- 2000-LOOKUP-XREF -------------------------------------------------------------------
    /**
     * {@code READ XREF-FILE ... KEY IS FD-XREF-CARD-NUM INVALID KEY ... NOT INVALID KEY ...}: a random READ
     * by key becomes a map lookup. A missing key is status 23, which takes the INVALID KEY branch; any
     * other non-00 status takes neither branch (the COBOL declares a FILE STATUS and no USE procedure,
     * so execution simply continues after the READ with WS-XREF-READ-STATUS still 0).
     */
    void lookupXref() {
        String fdXrefCardNum = cardXrefRecord.xrefCardNum();
        Optional<byte[]> rec;
        try {
            rec = xrefFile.read(fdXrefCardNum);
            xreffileStatus = rec.isPresent() ? "00" : FileStatusException.RECORD_NOT_FOUND;
        } catch (FileStatusException e) {
            xreffileStatus = e.status();
            rec = Optional.empty();
        }
        if (isInvalidKey(xreffileStatus)) {
            display.println("INVALID CARD NUMBER FOR XREF");
            wsXrefReadStatus = READ_FAILED;
        } else if ("00".equals(xreffileStatus)) {
            cardXrefRecord.moveFrom(rec.get());
            display.println("SUCCESSFUL READ OF XREF");
            display.println("CARD NUMBER: " + cardXrefRecord.xrefCardNum());
            display.println("ACCOUNT ID : " + cardXrefRecord.display(CardXrefRecord.XREF_ACCT_ID));
            display.println("CUSTOMER ID: " + cardXrefRecord.display(CardXrefRecord.XREF_CUST_ID));
        }
    }

    // ----- 3000-READ-ACCOUNT ------------------------------------------------------------------
    /** {@code READ ACCOUNT-FILE ... KEY IS FD-ACCT-ID}; same INVALID KEY mapping as {@link #lookupXref()}. */
    void readAccount() {
        String fdAcctId = String.format("%011d", accountRecord.acctId());
        Optional<byte[]> rec;
        try {
            rec = accountFile.read(fdAcctId);
            acctfileStatus = rec.isPresent() ? "00" : FileStatusException.RECORD_NOT_FOUND;
        } catch (FileStatusException e) {
            acctfileStatus = e.status();
            rec = Optional.empty();
        }
        if (isInvalidKey(acctfileStatus)) {
            display.println("INVALID ACCOUNT NUMBER FOUND");
            wsAcctReadStatus = READ_FAILED;
        } else if ("00".equals(acctfileStatus)) {
            accountRecord.moveFrom(rec.get());
            display.println("SUCCESSFUL READ OF ACCOUNT FILE");
        }
    }

    /** The INVALID KEY condition is raised for the 2x file statuses (21 sequence, 22 duplicate, 23 not found, 24 boundary). */
    private static boolean isInvalidKey(String status) {
        return status.charAt(0) == '2';
    }

    // ----- 0000-DALYTRAN-OPEN -----------------------------------------------------------------
    void dalytranOpen() {
        applResult = 8;
        try {
            dalytranFile.open();
            dalytranStatus = "00";
        } catch (FileStatusException e) {
            dalytranStatus = e.status();
        }
        applResult = "00".equals(dalytranStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING DAILY TRANSACTION FILE");
            ioStatus = dalytranStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 0100-CUSTFILE-OPEN -----------------------------------------------------------------
    void custfileOpen() {
        applResult = 8;
        try {
            customerFile.open();
            custfileStatus = "00";
        } catch (FileStatusException e) {
            custfileStatus = e.status();
        }
        applResult = "00".equals(custfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING CUSTOMER FILE");
            ioStatus = custfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 0200-XREFFILE-OPEN -----------------------------------------------------------------
    void xreffileOpen() {
        applResult = 8;
        try {
            xrefFile.open();
            xreffileStatus = "00";
        } catch (FileStatusException e) {
            xreffileStatus = e.status();
        }
        applResult = "00".equals(xreffileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING CROSS REF FILE");
            ioStatus = xreffileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 0300-CARDFILE-OPEN -----------------------------------------------------------------
    void cardfileOpen() {
        applResult = 8;
        try {
            cardFile.open();
            cardfileStatus = "00";
        } catch (FileStatusException e) {
            cardfileStatus = e.status();
        }
        applResult = "00".equals(cardfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING CARD FILE");
            ioStatus = cardfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 0400-ACCTFILE-OPEN -----------------------------------------------------------------
    void acctfileOpen() {
        applResult = 8;
        try {
            accountFile.open();
            acctfileStatus = "00";
        } catch (FileStatusException e) {
            acctfileStatus = e.status();
        }
        applResult = "00".equals(acctfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING ACCOUNT FILE");
            ioStatus = acctfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 0500-TRANFILE-OPEN -----------------------------------------------------------------
    void tranfileOpen() {
        applResult = 8;
        try {
            transactFile.open();
            tranfileStatus = "00";
        } catch (FileStatusException e) {
            tranfileStatus = e.status();
        }
        applResult = "00".equals(tranfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR OPENING TRANSACTION FILE");
            ioStatus = tranfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9000-DALYTRAN-CLOSE ----------------------------------------------------------------
    /**
     * Reproduces the COBOL paragraph as written: on a failed CLOSE of DALYTRAN it DISPLAYs
     * {@code ERROR CLOSING CUSTOMER FILE} and reports CUSTFILE-STATUS (not DALYTRAN-STATUS).
     */
    void dalytranClose() {
        applResult = 8;
        try {
            dalytranFile.close();
            dalytranStatus = "00";
        } catch (FileStatusException e) {
            dalytranStatus = e.status();
        }
        applResult = "00".equals(dalytranStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING CUSTOMER FILE");
            ioStatus = custfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9100-CUSTFILE-CLOSE ----------------------------------------------------------------
    void custfileClose() {
        applResult = 8;
        try {
            customerFile.close();
            custfileStatus = "00";
        } catch (FileStatusException e) {
            custfileStatus = e.status();
        }
        applResult = "00".equals(custfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING CUSTOMER FILE");
            ioStatus = custfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9200-XREFFILE-CLOSE ----------------------------------------------------------------
    void xreffileClose() {
        applResult = 8;
        try {
            xrefFile.close();
            xreffileStatus = "00";
        } catch (FileStatusException e) {
            xreffileStatus = e.status();
        }
        applResult = "00".equals(xreffileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING CROSS REF FILE");
            ioStatus = xreffileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9300-CARDFILE-CLOSE ----------------------------------------------------------------
    void cardfileClose() {
        applResult = 8;
        try {
            cardFile.close();
            cardfileStatus = "00";
        } catch (FileStatusException e) {
            cardfileStatus = e.status();
        }
        applResult = "00".equals(cardfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING CARD FILE");
            ioStatus = cardfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9400-ACCTFILE-CLOSE ----------------------------------------------------------------
    void acctfileClose() {
        applResult = 8;
        try {
            accountFile.close();
            acctfileStatus = "00";
        } catch (FileStatusException e) {
            acctfileStatus = e.status();
        }
        applResult = "00".equals(acctfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING ACCOUNT FILE");
            ioStatus = acctfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- 9500-TRANFILE-CLOSE ----------------------------------------------------------------
    void tranfileClose() {
        applResult = 8;
        try {
            transactFile.close();
            tranfileStatus = "00";
        } catch (FileStatusException e) {
            tranfileStatus = e.status();
        }
        applResult = "00".equals(tranfileStatus) ? 0 : 12;
        if (applResult != APPL_AOK) {
            display.println("ERROR CLOSING TRANSACTION FILE");
            ioStatus = tranfileStatus;
            displayIoStatus();
            abendProgram();
        }
    }

    // ----- Z-ABEND-PROGRAM --------------------------------------------------------------------
    void abendProgram() {
        display.println("ABENDING PROGRAM");
        display.flush();
        int timing = 0;
        int abcode = ABEND_CODE;
        throw new AbendException(abcode, timing, new FileStatusException(lastFailedDdname(), "I/O", ioStatus));
    }

    private String lastFailedDdname() {
        if (ioStatus.equals(dalytranStatus) && !"00".equals(dalytranStatus)) {
            return "DALYTRAN";
        }
        if (ioStatus.equals(custfileStatus) && !"00".equals(custfileStatus)) {
            return "CUSTFILE";
        }
        if (ioStatus.equals(xreffileStatus) && !"00".equals(xreffileStatus)) {
            return "XREFFILE";
        }
        if (ioStatus.equals(cardfileStatus) && !"00".equals(cardfileStatus)) {
            return "CARDFILE";
        }
        if (ioStatus.equals(acctfileStatus) && !"00".equals(acctfileStatus)) {
            return "ACCTFILE";
        }
        return "TRANFILE";
    }

    // ----- Z-DISPLAY-IO-STATUS ----------------------------------------------------------------
    void displayIoStatus() {
        String ioStatus04;
        if (!isNumeric(ioStatus) || ioStatus.charAt(0) == '9') {
            // IO-STATUS-0401 <- IO-STAT1; IO-STATUS-0403 <- binary value of the IO-STAT2 byte
            int twoBytesBinary = ioStatus.charAt(1) & 0xFF;
            ioStatus04 = ioStatus.charAt(0) + String.format("%03d", twoBytesBinary % 1000);
        } else {
            ioStatus04 = "00" + ioStatus;
        }
        display.println("FILE STATUS IS: NNNN" + ioStatus04);
    }

    private static boolean isNumeric(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                return false;
            }
        }
        return true;
    }

    /** One row per DALYTRAN record, in file order (the post-EOF duplicate lookup is not included). */
    public List<TransactionOutcome> outcomes() {
        return Collections.unmodifiableList(outcomes);
    }

    /** The current FILE STATUS of DALYTRAN (for tests). */
    public String dalytranStatus() {
        return dalytranStatus;
    }

    /** The current FILE STATUS of XREFFILE (for tests). */
    public String xreffileStatus() {
        return xreffileStatus;
    }

    /** The current FILE STATUS of ACCTFILE (for tests). */
    public String acctfileStatus() {
        return acctfileStatus;
    }

    /** The current FILE STATUS of TRANFILE (for tests). */
    public String tranfileStatus() {
        return tranfileStatus;
    }
}
