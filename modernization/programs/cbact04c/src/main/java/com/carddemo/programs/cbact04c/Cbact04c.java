package com.carddemo.programs.cbact04c;

import com.carddemo.programs.cbact04c.copybook.AccountRecord;
import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import com.carddemo.programs.cbact04c.copybook.DisGroupRecord;
import com.carddemo.programs.cbact04c.copybook.Pic;
import com.carddemo.programs.cbact04c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbact04c.copybook.TranRecord;
import com.carddemo.programs.cbact04c.io.FileStatus;
import com.carddemo.programs.cbact04c.io.OpenMode;
import com.carddemo.programs.cbact04c.io.ReadResult;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Locale;
import java.util.function.Consumer;

/**
 * CBACT04C - CardDemo interest calculator (batch). Reads the transaction category balance file in key
 * order, looks up the disclosure-group interest rate for each category, writes one interest transaction
 * per non-zero rate and adds the accumulated interest to the account's current balance.
 * <p>
 * Paragraphs are methods with the same name; DISPLAY output goes to the injected display sink;
 * 9999-ABEND-PROGRAM throws {@link AbendException}. Semantics intentionally kept from the COBOL source,
 * including its quirks (see README / PR description): the account of the last TCATBALF group is never
 * rewritten, because the PERFORM UNTIL test happens before the ELSE branch that would update it can run.
 */
public class Cbact04c {

    static final int ABEND_CODE = 999;

    private final Cbact04cFiles files;
    private final Clock clock;
    private final Consumer<String> display;

    // WORKING-STORAGE
    private TranCatBalRecord tranCatBalRecord;
    private CardXrefRecord cardXrefRecord;
    private DisGroupRecord disGroupRecord;
    private AccountRecord accountRecord;
    private TranRecord tranRecord = TranRecord.blank();
    private String ioStatus = "00";
    private final ApplResult applResult = new ApplResult();
    private char endOfFile = 'N';
    private String wsLastAcctNum = " ".repeat(11);
    private BigDecimal wsMonthlyInt = BigDecimal.ZERO.setScale(2);
    private BigDecimal wsTotalInt = BigDecimal.ZERO.setScale(2);
    private char wsFirstTime = 'Y';
    private long wsRecordCount;
    private int wsTranidSuffix;
    private String parmDate;

    // FD record keys (FD-ACCT-ID, FD-XREF-ACCT-ID, FD-DISCGRP-KEY)
    private long fdAcctId;
    private long fdXrefAcctId;
    private String fdDisAcctGroupId;
    private String fdDisTranTypeCd;
    private int fdDisTranCatCd;

    public Cbact04c(Cbact04cFiles files, Clock clock, Consumer<String> display) {
        this.files = files;
        this.clock = clock;
        this.display = display;
    }

    /**
     * PROCEDURE DIVISION USING EXTERNAL-PARMS. {@code parm} is the JCL PARM (PARM-DATE PIC X(10)).
     *
     * @return RETURN-CODE at GOBACK (always 0; failures abend)
     */
    public int run(String parm) {
        parmDate = Pic.alpha(parm, 10);
        display.accept("START OF EXECUTION OF PROGRAM CBACT04C");
        tcatbalfOpen0000();
        xreffileOpen0100();
        discgrpOpen0200();
        acctfileOpen0300();
        tranfileOpen0400();

        while (endOfFile != 'Y') {
            if (endOfFile == 'N') {
                tcatbalfGetNext1000();
                if (endOfFile == 'N') {
                    wsRecordCount = (wsRecordCount + 1) % 1_000_000_000L;
                    display.accept(TranCatBalRecord.CODEC.encode(tranCatBalRecord));
                    if (!Pic.unsigned(tranCatBalRecord.trancatAcctId(), 11).equals(wsLastAcctNum)) {
                        if (wsFirstTime != 'Y') {
                            updateAccount1050();
                        } else {
                            wsFirstTime = 'N';
                        }
                        wsTotalInt = BigDecimal.ZERO.setScale(2);
                        wsLastAcctNum = Pic.unsigned(tranCatBalRecord.trancatAcctId(), 11);
                        fdAcctId = tranCatBalRecord.trancatAcctId();
                        getAcctData1100();
                        fdXrefAcctId = tranCatBalRecord.trancatAcctId();
                        getXrefData1110();
                    }
                    fdDisAcctGroupId = accountRecord.acctGroupId();
                    fdDisTranCatCd = tranCatBalRecord.trancatCd();
                    fdDisTranTypeCd = tranCatBalRecord.trancatTypeCd();
                    getInterestRate1200();
                    if (disGroupRecord.disIntRate().signum() != 0) {
                        computeInterest1300();
                        computeFees1400();
                    }
                }
            } else {
                updateAccount1050();
            }
        }

        tcatbalfClose9000();
        xreffileClose9100();
        discgrpClose9200();
        acctfileClose9300();
        tranfileClose9400();

        display.accept("END OF EXECUTION OF PROGRAM CBACT04C");
        return 0;
    }

    // 0000-TCATBALF-OPEN
    void tcatbalfOpen0000() {
        checkOpen(files.tcatbalFile().open(), "ERROR OPENING TRANSACTION CATEGORY BALANCE");
    }

    // 0100-XREFFILE-OPEN
    void xreffileOpen0100() {
        String status = files.xrefFile().open(OpenMode.INPUT);
        checkOpen(status, "ERROR OPENING CROSS REF FILE" + status);
    }

    // 0200-DISCGRP-OPEN (the COBOL message really says "DALY REJECTS")
    void discgrpOpen0200() {
        checkOpen(files.discgrpFile().open(OpenMode.INPUT), "ERROR OPENING DALY REJECTS FILE");
    }

    // 0300-ACCTFILE-OPEN
    void acctfileOpen0300() {
        checkOpen(files.accountFile().open(OpenMode.I_O), "ERROR OPENING ACCOUNT MASTER FILE");
    }

    // 0400-TRANFILE-OPEN
    void tranfileOpen0400() {
        checkOpen(files.transactFile().open(), "ERROR OPENING TRANSACTION FILE");
    }

    private void checkOpen(String status, String message) {
        applResult.set(8);
        applResult.set(FileStatus.OK.equals(status) ? 0 : 12);
        if (!applResult.isApplAok()) {
            display.accept(message);
            ioStatus = status;
            displayIoStatus9910();
            abendProgram9999(message);
        }
    }

    // 1000-TCATBALF-GET-NEXT
    void tcatbalfGetNext1000() {
        ReadResult<TranCatBalRecord> rr = files.tcatbalFile().read();
        String status = rr.status();
        if (FileStatus.OK.equals(status)) {
            tranCatBalRecord = rr.record();
            applResult.set(0);
        } else if (FileStatus.END_OF_FILE.equals(status)) {
            applResult.set(16);
        } else {
            applResult.set(12);
        }
        if (!applResult.isApplAok()) {
            if (applResult.isApplEof()) {
                endOfFile = 'Y';
            } else {
                failIo("ERROR READING TRANSACTION CATEGORY FILE", status);
            }
        }
    }

    // 1050-UPDATE-ACCOUNT
    void updateAccount1050() {
        accountRecord = accountRecord
                .withAcctCurrBal(Pic.truncate(accountRecord.acctCurrBal().add(wsTotalInt), 10, 2))
                .withAcctCurrCycCredit(BigDecimal.ZERO)
                .withAcctCurrCycDebit(BigDecimal.ZERO);
        String status = files.accountFile().rewrite(accountRecord);
        applResult.set(FileStatus.OK.equals(status) ? 0 : 12);
        if (!applResult.isApplAok()) {
            failIo("ERROR RE-WRITING ACCOUNT FILE", status);
        }
    }

    // 1100-GET-ACCT-DATA
    void getAcctData1100() {
        ReadResult<AccountRecord> rr = files.accountFile().read(fdAcctId);
        String status = rr.status();
        if (FileStatus.isInvalidKey(status)) {
            display.accept("ACCOUNT NOT FOUND: " + Pic.unsigned(fdAcctId, 11));
        }
        if (FileStatus.OK.equals(status)) {
            accountRecord = rr.record();
            applResult.set(0);
        } else {
            applResult.set(12);
        }
        if (!applResult.isApplAok()) {
            failIo("ERROR READING ACCOUNT FILE", status);
        }
    }

    // 1110-GET-XREF-DATA (READ XREF-FILE KEY IS FD-XREF-ACCT-ID)
    void getXrefData1110() {
        ReadResult<CardXrefRecord> rr = files.xrefFile().read(fdXrefAcctId);
        String status = rr.status();
        if (FileStatus.isInvalidKey(status)) {
            display.accept("ACCOUNT NOT FOUND: " + Pic.unsigned(fdXrefAcctId, 11));
        }
        if (FileStatus.OK.equals(status)) {
            cardXrefRecord = rr.record();
            applResult.set(0);
        } else {
            applResult.set(12);
        }
        if (!applResult.isApplAok()) {
            failIo("ERROR READING XREF FILE", status);
        }
    }

    // 1200-GET-INTEREST-RATE
    void getInterestRate1200() {
        ReadResult<DisGroupRecord> rr = files.discgrpFile().read(discgrpKey());
        String status = rr.status();
        if (FileStatus.isInvalidKey(status)) {
            display.accept("DISCLOSURE GROUP RECORD MISSING");
            display.accept("TRY WITH DEFAULT GROUP CODE");
        }
        if (FileStatus.OK.equals(status)) {
            disGroupRecord = rr.record();
        }
        if (FileStatus.OK.equals(status) || FileStatus.RECORD_NOT_FOUND.equals(status)) {
            applResult.set(0);
        } else {
            applResult.set(12);
        }
        if (!applResult.isApplAok()) {
            failIo("ERROR READING DISCLOSURE GROUP FILE", status);
        }
        if (FileStatus.RECORD_NOT_FOUND.equals(status)) {
            fdDisAcctGroupId = Pic.alpha("DEFAULT", 10);
            getDefaultIntRate1200A();
        }
    }

    // 1200-A-GET-DEFAULT-INT-RATE
    void getDefaultIntRate1200A() {
        ReadResult<DisGroupRecord> rr = files.discgrpFile().read(discgrpKey());
        String status = rr.status();
        if (FileStatus.OK.equals(status)) {
            disGroupRecord = rr.record();
            applResult.set(0);
        } else {
            applResult.set(12);
        }
        if (!applResult.isApplAok()) {
            failIo("ERROR READING DEFAULT DISCLOSURE GROUP", status);
        }
    }

    private DisGroupRecord.Key discgrpKey() {
        return new DisGroupRecord.Key(fdDisAcctGroupId, fdDisTranTypeCd, fdDisTranCatCd);
    }

    // 1300-COMPUTE-INTEREST: COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200 (no ROUNDED)
    void computeInterest1300() {
        wsMonthlyInt = computeMonthlyInterest(tranCatBalRecord.tranCatBal(), disGroupRecord.disIntRate());
        wsTotalInt = Pic.truncate(wsTotalInt.add(wsMonthlyInt), 9, 2);
        writeTx1300B();
    }

    /** (balance * annual rate %) / 1200, truncated into PIC S9(09)V99. */
    static BigDecimal computeMonthlyInterest(BigDecimal tranCatBal, BigDecimal disIntRate) {
        BigDecimal product = tranCatBal.multiply(disIntRate);
        BigDecimal quotient = product.divide(BigDecimal.valueOf(1200), 10, RoundingMode.DOWN);
        return Pic.truncate(quotient, 9, 2);
    }

    // 1300-B-WRITE-TX
    void writeTx1300B() {
        wsTranidSuffix = (wsTranidSuffix + 1) % 1_000_000;
        String tranId = Pic.stringInto(tranRecord.tranId(), 16, parmDate, Pic.unsigned(wsTranidSuffix, 6));
        String tranDesc = Pic.stringInto(tranRecord.tranDesc(), 100,
                "Int. for a/c ", Pic.unsigned(accountRecord.acctId(), 11));
        String ts = getDb2FormatTimestamp();
        tranRecord = new TranRecord(
                tranId,
                "01",
                5, // MOVE '05' TO TRAN-CAT-CD PIC 9(04) -> 0005
                "System",
                tranDesc,
                wsMonthlyInt,
                0,
                "",
                "",
                "",
                cardXrefRecord.xrefCardNum(),
                ts,
                ts,
                tranRecord.filler());
        String status = files.transactFile().write(tranRecord);
        applResult.set(FileStatus.OK.equals(status) ? 0 : 12);
        if (!applResult.isApplAok()) {
            failIo("ERROR WRITING TRANSACTION RECORD", status);
        }
    }

    // 1400-COMPUTE-FEES: "To be implemented" in the COBOL source
    void computeFees1400() {
    }

    // 9000-TCATBALF-CLOSE
    void tcatbalfClose9000() {
        checkClose(files.tcatbalFile().close(), "ERROR CLOSING TRANSACTION BALANCE FILE");
    }

    // 9100-XREFFILE-CLOSE
    void xreffileClose9100() {
        checkClose(files.xrefFile().close(), "ERROR CLOSING CROSS REF FILE");
    }

    // 9200-DISCGRP-CLOSE
    void discgrpClose9200() {
        checkClose(files.discgrpFile().close(), "ERROR CLOSING DISCLOSURE GROUP FILE");
    }

    // 9300-ACCTFILE-CLOSE
    void acctfileClose9300() {
        checkClose(files.accountFile().close(), "ERROR CLOSING ACCOUNT FILE");
    }

    // 9400-TRANFILE-CLOSE
    void tranfileClose9400() {
        checkClose(files.transactFile().close(), "ERROR CLOSING TRANSACTION FILE");
    }

    private void checkClose(String status, String message) {
        applResult.set(8);
        applResult.set(FileStatus.OK.equals(status) ? 0 : 12);
        if (!applResult.isApplAok()) {
            failIo(message, status);
        }
    }

    private void failIo(String message, String status) {
        display.accept(message);
        ioStatus = status;
        displayIoStatus9910();
        abendProgram9999(message);
    }

    // Z-GET-DB2-FORMAT-TIMESTAMP: FUNCTION CURRENT-DATE -> 'YYYY-MM-DD-HH.MM.SS.hh0000'
    String getDb2FormatTimestamp() {
        LocalDateTime now = LocalDateTime.now(clock);
        return String.format(Locale.ROOT, "%04d-%02d-%02d-%02d.%02d.%02d.%02d0000",
                now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000);
    }

    // 9999-ABEND-PROGRAM
    private void abendProgram9999(String reason) {
        display.accept("ABENDING PROGRAM");
        throw new AbendException(ABEND_CODE, 0, reason);
    }

    // 9910-DISPLAY-IO-STATUS
    void displayIoStatus9910() {
        display.accept("FILE STATUS IS: NNNN" + formatIoStatus(ioStatus));
    }

    /** IO-STATUS-04 as built by 9910-DISPLAY-IO-STATUS. */
    static String formatIoStatus(String ioStatus) {
        String s = Pic.alpha(ioStatus, 2);
        char stat1 = s.charAt(0);
        char stat2 = s.charAt(1);
        boolean numeric = Character.isDigit(stat1) && Character.isDigit(stat2);
        if (!numeric || stat1 == '9') {
            // TWO-BYTES-BINARY PIC 9(4) BINARY with IO-STAT2 in the low-order byte -> IO-STATUS-0403 PIC 999
            int binary = (stat2 & 0xFF) % 1000;
            return stat1 + String.format("%03d", binary);
        }
        return "00" + s;
    }

    // Accessors for tests / callers
    long recordCount() {
        return wsRecordCount;
    }

    int tranidSuffix() {
        return wsTranidSuffix;
    }
}
