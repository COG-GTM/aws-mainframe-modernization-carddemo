package com.carddemo.interestcalc.program;

import com.carddemo.interestcalc.copybook.AccountRecord;
import com.carddemo.interestcalc.copybook.CardXrefRecord;
import com.carddemo.interestcalc.copybook.CobolNumeric;
import com.carddemo.interestcalc.copybook.DisclosureGroupRecord;
import com.carddemo.interestcalc.copybook.FixedWidth;
import com.carddemo.interestcalc.copybook.TranCatBalanceRecord;
import com.carddemo.interestcalc.copybook.TransactionRecord;
import com.carddemo.interestcalc.file.AccountFile;
import com.carddemo.interestcalc.file.CardXrefFile;
import com.carddemo.interestcalc.file.DisclosureGroupFile;
import com.carddemo.interestcalc.file.FileStatus;
import com.carddemo.interestcalc.file.KeyedRead;
import com.carddemo.interestcalc.file.TranCatBalanceFile;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Java 21 migration of the batch COBOL program {@code app/cbl/CBACT04C.cbl} - the CardDemo
 * interest calculator. The COBOL remains the system of record and is not modified; this class
 * reproduces its observable behaviour paragraph by paragraph.
 *
 * <p><b>Decimal semantics.</b> Every monetary value is a {@link BigDecimal} with an explicit
 * scale; {@code float}/{@code double} appear nowhere in this module. {@code 1300-COMPUTE-INTEREST}
 * has no {@code ROUNDED} phrase, so the result is truncated toward zero, which is
 * {@link RoundingMode#DOWN} (see {@link CobolNumeric}).
 *
 * <p><b>Nondeterminism.</b> The only nondeterministic outputs are {@code TRAN-ORIG-TS} and
 * {@code TRAN-PROC-TS}, which come from {@code FUNCTION CURRENT-DATE}. They are produced from an
 * injected {@link Clock} so a test can pin them.
 *
 * <p>This class is deliberately stateful and single-use, mirroring the WORKING-STORAGE of the
 * COBOL program; create one per run.
 */
public final class Cbact04cProgram {

    /** The literal divisor of {@code 1300-COMPUTE-INTEREST}: 12 months x 100 percent. */
    private static final BigDecimal MONTHLY_RATE_DIVISOR = new BigDecimal("1200");

    private static final DateTimeFormatter DB2_TIMESTAMP_HEAD =
            DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss");

    private final Clock clock;

    // --- WORKING-STORAGE ----------------------------------------------------
    /** {@code WS-LAST-ACCT-NUM PIC X(11) VALUE SPACES}. */
    private String lastAccountNumber = " ".repeat(11);
    /** {@code WS-MONTHLY-INT PIC S9(09)V99}. */
    private BigDecimal monthlyInterest = zero();
    /** {@code WS-TOTAL-INT PIC S9(09)V99}. */
    private BigDecimal totalInterest = zero();
    /** {@code WS-FIRST-TIME PIC X(01) VALUE 'Y'}. */
    private boolean firstTime = true;
    /** {@code WS-RECORD-COUNT PIC 9(09) VALUE 0}. */
    private long recordCount;
    /** {@code WS-TRANID-SUFFIX PIC 9(06) VALUE 0}. */
    private long tranIdSuffix;
    /** {@code END-OF-FILE PIC X(01) VALUE 'N'}. */
    private boolean endOfFile;
    /** {@code DB2-FORMAT-TS PIC X(26)}. */
    private String db2FormatTimestamp = " ".repeat(26);

    /** The record areas the COBOL program keeps between paragraphs. */
    private AccountRecord accountRecord;
    private CardXrefRecord cardXrefRecord;
    private DisclosureGroupRecord disclosureGroupRecord;
    private TranCatBalanceRecord tranCatBalanceRecord;

    // --- files and outputs --------------------------------------------------
    private TranCatBalanceFile tranCatBalanceFile;
    private CardXrefFile cardXrefFile;
    private AccountFile accountFile;
    private DisclosureGroupFile disclosureGroupFile;
    private final List<TransactionRecord> transactions = new ArrayList<>();
    private final List<String> console = new ArrayList<>();
    private String runDate;

    public Cbact04cProgram(Clock clock) {
        this.clock = clock;
    }

    /**
     * Implements the CBACT04C PROCEDURE DIVISION mainline: open all five files, drive the
     * TCATBALF sequential read loop, then close.
     */
    public InterestCalculationResult run(InterestCalculationRequest request) {
        runDate = request.runDate();
        display("START OF EXECUTION OF PROGRAM CBACT04C");

        tranCatBalanceFile = TranCatBalanceFile.load(request.tranCatBalanceFile());  // 0000-TCATBALF-OPEN
        cardXrefFile = CardXrefFile.load(request.cardXrefFile());                    // 0100-XREFFILE-OPEN
        disclosureGroupFile = DisclosureGroupFile.load(request.disclosureGroupFile());// 0200-DISCGRP-OPEN
        accountFile = AccountFile.load(request.accountFile());                       // 0300-ACCTFILE-OPEN
        // 0400-TRANFILE-OPEN: TRANSACT is OPEN OUTPUT, i.e. always created empty.

        while (!endOfFile) {
            if (!endOfFile) {
                getNextTranCatBalanceRecord();
                if (!endOfFile) {
                    recordCount++;
                    display(tranCatBalanceRecord.format());
                    if (!tranCatBalanceRecord.accountId().equals(lastAccountNumber)) {
                        if (!firstTime) {
                            updateAccount();
                        } else {
                            firstTime = false;
                        }
                        totalInterest = zero();
                        lastAccountNumber = tranCatBalanceRecord.accountId();
                        getAccountData(tranCatBalanceRecord.accountId());
                        getCardXrefData(tranCatBalanceRecord.accountId());
                    }
                    getInterestRate();
                    if (disclosureGroupRecord.interestRate().signum() != 0) {
                        computeInterest();
                        computeFees();
                    }
                }
            } else {
                // Faithful translation of the COBOL ELSE branch. It is unreachable in COBOL
                // too: the PERFORM UNTIL condition is evaluated before each iteration, so the
                // loop has already exited by the time END-OF-FILE is 'Y'. The consequence is
                // that the LAST account group processed is never written back by
                // 1050-UPDATE-ACCOUNT. See README.md - this is preserved, not fixed.
                updateAccount();
            }
        }

        display("END OF EXECUTION OF PROGRAM CBACT04C");
        return new InterestCalculationResult(List.copyOf(transactions), accountFile.unload(), recordCount,
                List.copyOf(console));
    }

    /** Implements CBACT04C 1000-TCATBALF-GET-NEXT. */
    private void getNextTranCatBalanceRecord() {
        KeyedRead<TranCatBalanceRecord> read = tranCatBalanceFile.readNext();
        if (read.ok()) {
            tranCatBalanceRecord = read.record();
            return;
        }
        if (FileStatus.END_OF_FILE.equals(read.status())) {
            endOfFile = true;
            return;
        }
        display("ERROR READING TRANSACTION CATEGORY FILE");
        abend(read.status());
    }

    /**
     * Implements CBACT04C 1050-UPDATE-ACCOUNT: add the accumulated interest to
     * {@code ACCT-CURR-BAL}, zero the current cycle credit and debit, and rewrite the record.
     */
    private void updateAccount() {
        AccountRecord updated = accountRecord.withBalanceAndClearedCycleTotals(
                accountRecord.currentBalance().add(totalInterest));
        accountRecord = updated;
        String status = accountFile.rewrite(updated);
        if (!FileStatus.OK.equals(status)) {
            display("ERROR RE-WRITING ACCOUNT FILE");
            abend(status);
        }
    }

    /** Implements CBACT04C 1100-GET-ACCT-DATA. */
    private void getAccountData(String accountId) {
        KeyedRead<AccountRecord> read = accountFile.read(accountId);
        if (!read.ok()) {
            display("ACCOUNT NOT FOUND: " + accountId);
            display("ERROR READING ACCOUNT FILE");
            abend(read.status());
        }
        accountRecord = read.record();
    }

    /** Implements CBACT04C 1110-GET-XREF-DATA. */
    private void getCardXrefData(String accountId) {
        KeyedRead<CardXrefRecord> read = cardXrefFile.readByAccountId(accountId);
        if (!read.ok()) {
            display("ACCOUNT NOT FOUND: " + accountId);
            display("ERROR READING XREF FILE");
            abend(read.status());
        }
        cardXrefRecord = read.record();
    }

    /**
     * Implements CBACT04C 1200-GET-INTEREST-RATE.
     *
     * <p>The key is the account's disclosure group plus the transaction type and category of the
     * balance record. File status {@code '23'} (record not found) is not an error: the read is
     * retried with the account group id {@code 'DEFAULT'}.
     */
    private void getInterestRate() {
        String key = DisclosureGroupRecord.key(accountRecord.groupId(),
                tranCatBalanceRecord.typeCode(), tranCatBalanceRecord.categoryCode());
        KeyedRead<DisclosureGroupRecord> read = disclosureGroupFile.read(key);
        if (read.ok()) {
            disclosureGroupRecord = read.record();
        } else {
            display("DISCLOSURE GROUP RECORD MISSING");
            display("TRY WITH DEFAULT GROUP CODE");
        }
        if (!read.ok() && !FileStatus.NOT_FOUND.equals(read.status())) {
            display("ERROR READING DISCLOSURE GROUP FILE");
            abend(read.status());
        }
        if (FileStatus.NOT_FOUND.equals(read.status())) {
            getDefaultInterestRate();
        }
    }

    /** Implements CBACT04C 1200-A-GET-DEFAULT-INT-RATE. */
    private void getDefaultInterestRate() {
        String key = DisclosureGroupRecord.key("DEFAULT",
                tranCatBalanceRecord.typeCode(), tranCatBalanceRecord.categoryCode());
        KeyedRead<DisclosureGroupRecord> read = disclosureGroupFile.read(key);
        if (!read.ok()) {
            display("ERROR READING DEFAULT DISCLOSURE GROUP");
            abend(read.status());
        }
        disclosureGroupRecord = read.record();
    }

    /**
     * Implements CBACT04C 1300-COMPUTE-INTEREST.
     *
     * <pre>
     * COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
     * ADD WS-MONTHLY-INT TO WS-TOTAL-INT
     * </pre>
     *
     * <p>There is no {@code ROUNDED} phrase on the {@code COMPUTE}, so the quotient is truncated
     * toward zero into {@code WS-MONTHLY-INT PIC S9(09)V99}: {@link RoundingMode#DOWN}, not
     * {@code HALF_UP} (which would round 1.256 up to 1.26) and not {@code FLOOR} (which would
     * take -1.256 down to -1.26). The product is exact - two scale-2 operands give a scale-4
     * product - so the only rounding in the whole calculation happens on the division.
     */
    private void computeInterest() {
        BigDecimal product = tranCatBalanceRecord.balance().multiply(disclosureGroupRecord.interestRate());
        BigDecimal quotient = product.divide(MONTHLY_RATE_DIVISOR, 2, RoundingMode.DOWN);
        monthlyInterest = CobolNumeric.store(quotient, 9, 2);
        totalInterest = CobolNumeric.store(totalInterest.add(monthlyInterest), 9, 2);
        writeTransaction();
    }

    /** Implements CBACT04C 1300-B-WRITE-TX. */
    private void writeTransaction() {
        tranIdSuffix = (tranIdSuffix + 1) % 1_000_000L;  // PIC 9(06) wraps, it does not overflow
        String transactionId = runDate + String.format("%06d", tranIdSuffix);

        getDb2FormatTimestamp();
        TransactionRecord record = new TransactionRecord(
                transactionId,
                "01",
                "0005",
                "System",
                // STRING 'Int. for a/c ' ACCT-ID DELIMITED BY SIZE INTO TRAN-DESC.
                FixedWidth.alphanumeric("Int. for a/c " + accountRecord.accountId(), 100),
                monthlyInterest,
                "000000000",
                "",
                "",
                "",
                cardXrefRecord.cardNumber(),
                db2FormatTimestamp,
                db2FormatTimestamp);
        transactions.add(record);
    }

    /**
     * Implements CBACT04C 1400-COMPUTE-FEES.
     *
     * <p>Intentionally empty: the COBOL paragraph is a stub whose entire body is a comment
     * ("To be implemented") followed by {@code EXIT}. No fee logic has been invented here.
     */
    private void computeFees() {
        // To be implemented - stub in CBACT04C, stub here.
    }

    /**
     * Implements CBACT04C Z-GET-DB2-FORMAT-TIMESTAMP: reformat {@code FUNCTION CURRENT-DATE}
     * into the 26 character DB2 timestamp {@code YYYY-MM-DD-HH.MM.SS.MM0000}, where {@code MM}
     * is the hundredths of a second returned by {@code CURRENT-DATE} and the last four digits
     * are the literal {@code '0000'} the paragraph moves into {@code DB2-REST}.
     */
    private void getDb2FormatTimestamp() {
        LocalDateTime now = LocalDateTime.now(clock);
        int hundredths = now.getNano() / 10_000_000;
        db2FormatTimestamp = DB2_TIMESTAMP_HEAD.format(now) + "." + String.format("%02d", hundredths) + "0000";
    }

    /** Implements CBACT04C 9999-ABEND-PROGRAM (via 9910-DISPLAY-IO-STATUS). */
    private void abend(String fileStatus) {
        display("FILE STATUS IS: NNNN00" + fileStatus);
        display("ABENDING PROGRAM");
        throw new AbendException("file status " + fileStatus);
    }

    private void display(String message) {
        console.add(message);
    }

    private static BigDecimal zero() {
        return BigDecimal.ZERO.setScale(2);
    }
}
