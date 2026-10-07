package com.carddemo.batch.intcalc;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.RecordSink;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.posttran.Cbtrn02c;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.CobolNumeric;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.transaction.DisclosureGroupId;
import com.carddemo.transaction.DisclosureGroupRecord;
import com.carddemo.transaction.DisclosureGroupRepository;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Locale;
import java.util.Optional;

/**
 * {@code CBACT04C} (INTCALC STEP15): reads TCATBALF in key order; on each change of TRANCAT-ACCT-ID rewrites the
 * previous account ({@code 1050-UPDATE-ACCOUNT}: ACCT-CURR-BAL + total interest, cycle credit/debit reset to 0), then
 * reads the new account (ACCTFILE) and its first card (XREFFILE by the CXACAIX alternate key XREF-ACCT-ID). Each row's
 * rate comes from DISCGRP keyed by ACCT-GROUP-ID + TRANCAT-TYPE-CD + TRANCAT-CD, re-read with group {@code DEFAULT}
 * when missing (the lookup of {@link DisclosureGroupRepository#findWithDefault}, done as two reads so the SYSOUT
 * lines between them are kept). A non-zero rate writes one SYSTRAN record: id = PARM-DATE (10) + a 6-digit suffix
 * counted over the run, type 01 / category 05, amount {@code TRAN-CAT-BAL * DIS-INT-RATE / 1200} truncated into
 * {@code S9(09)V99} (no ROUNDED, ADR-0004/0005). As in the COBOL, the account of the last TCATBALF row is never
 * rewritten: end of file leaves the loop before {@code 1050-UPDATE-ACCOUNT} runs again (rules doc CBACT04C.md).
 */
public final class Cbact04c {

    public static final String PROGRAM = "CBACT04C";
    public static final String TCATBALF = "TCATBALF";
    public static final String XREFFILE = "XREFFILE";
    public static final String DISCGRP = "DISCGRP";
    public static final String ACCTFILE = "ACCTFILE";
    public static final String TRANSACT = "TRANSACT";
    public static final int TRANSACT_LRECL = 350;
    public static final String INTEREST_TYPE_CD = "01";
    public static final int INTEREST_CAT_CD = 5;
    public static final String SOURCE = "System";

    private static final BigDecimal MONTHS_TIMES_PERCENT = BigDecimal.valueOf(1200);
    private static final int SUFFIX_MODULUS = 1_000_000;

    /** {@code read}: TCATBALF records; {@code written}: SYSTRAN records; {@code accountsUpdated}: REWRITEs. */
    public record Result(long read, long written, long accountsUpdated, ReturnCode returnCode) {
    }

    private final KsdsInput tcatbalf;
    private final KeyedDataset<Long, CardXrefRecord> xreffile;
    private final KeyedDataset<DisclosureGroupId, DisclosureGroupRecord> discgrp;
    private final KeyedDataset<Long, AccountRecord> acctfile;
    private final RecordSink transact;
    private final String parmDate;
    private final RecordEncoding encoding;
    private final Sysout sysout;
    private final Clock clock;

    /**
     * @param xreffile CARDXREF read by XREF-ACCT-ID (the CXACAIX path), see {@link XrefByAccount}
     * @param parmDate the JCL {@code PARM} (PARM-DATE {@code X(10)}: padded with spaces / cut to 10)
     * @param encoding code page of the SYSTRAN records
     */
    public Cbact04c(KsdsInput tcatbalf, KeyedDataset<Long, CardXrefRecord> xreffile,
                    KeyedDataset<DisclosureGroupId, DisclosureGroupRecord> discgrp,
                    KeyedDataset<Long, AccountRecord> acctfile, RecordSink transact, String parmDate,
                    RecordEncoding encoding, Sysout sysout, Clock clock) {
        this.tcatbalf = tcatbalf;
        this.xreffile = xreffile;
        this.discgrp = discgrp;
        this.acctfile = acctfile;
        this.transact = transact;
        this.parmDate = parm(parmDate);
        this.encoding = encoding;
        this.sysout = sysout;
        this.clock = clock;
    }

    public Result run() {
        sysout.display("START OF EXECUTION OF PROGRAM " + PROGRAM);
        long read = 0;
        long written = 0;
        long updated = 0;
        long suffix = 0;
        Long lastAcctId = null;
        AccountRecord account = null;
        CardXrefRecord xref = null;
        BigDecimal totalInterest = BigDecimal.ZERO;
        try {
            io(tcatbalf::open, "ERROR OPENING TRANSACTION CATEGORY BALANCE");
            open(xreffile::open, "ERROR OPENING CROSS REF FILE");
            // 0200-DISCGRP-OPEN displays the DALYREJS message of the program it was copied from.
            io(discgrp::open, "ERROR OPENING DALY REJECTS FILE");
            io(acctfile::open, "ERROR OPENING ACCOUNT MASTER FILE");
            io(transact::open, "ERROR OPENING TRANSACTION FILE");
            while (true) {
                Optional<FixedWidthRecord> next;
                try {
                    next = tcatbalf.readNext();
                } catch (FileStatusException e) {
                    throw sysout.ioAbend("ERROR READING TRANSACTION CATEGORY FILE", e);
                }
                if (next.isEmpty()) {
                    break;
                }
                read++;
                sysout.display(next.get().text());
                TranCatBalanceRecord balance = TranCatBalanceRecord.MAPPER.fromRecord(next.get());
                if (lastAcctId == null || balance.acctId() != lastAcctId) {
                    if (lastAcctId != null) {
                        updateAccount(account, totalInterest);
                        updated++;
                    }
                    totalInterest = BigDecimal.ZERO;
                    lastAcctId = balance.acctId();
                    account = readAccount(balance.acctId());
                    xref = readXref(balance.acctId());
                }
                BigDecimal rate = interestRate(account.groupId(), balance.tranTypeCd(), balance.tranCatCd());
                if (rate.signum() != 0) {
                    BigDecimal interest = monthlyInterest(balance.balance(), rate).add(new BigDecimal("0.01")); // SCRATCH: golden-set failure demo, never merge
                    totalInterest = CobolNumeric.truncate(totalInterest.add(interest), 11, 2, true);
                    suffix = (suffix + 1) % SUFFIX_MODULUS;
                    writeTransaction(transaction(tranId(parmDate, suffix), account.acctId(), interest,
                            xref.cardNum(), Cbtrn02c.PROC_TS.format(LocalDateTime.now(clock))));
                    written++;
                }
            }
            io(tcatbalf::close, "ERROR CLOSING TRANSACTION BALANCE FILE");
            io(xreffile::close, "ERROR CLOSING CROSS REF FILE");
            io(discgrp::close, "ERROR CLOSING DISCLOSURE GROUP FILE");
            io(acctfile::close, "ERROR CLOSING ACCOUNT FILE");
            io(transact::close, "ERROR CLOSING TRANSACTION FILE");
        } catch (RuntimeException abend) {
            flushOnAbend(abend);
            throw abend;
        }
        sysout.display("END OF EXECUTION OF PROGRAM " + PROGRAM);
        return new Result(read, written, updated, ReturnCode.OK);
    }

    /** {@code 1300-COMPUTE-INTEREST}: {@code ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200} into {@code S9(09)V99}. */
    public static BigDecimal monthlyInterest(BigDecimal balance, BigDecimal rate) {
        BigDecimal quotient = balance.multiply(rate).divide(MONTHS_TIMES_PERCENT, 2, RoundingMode.DOWN);
        return CobolNumeric.truncate(quotient, 11, 2, true);
    }

    /** {@code STRING PARM-DATE, WS-TRANID-SUFFIX DELIMITED BY SIZE INTO TRAN-ID}. */
    public static String tranId(String parmDate, long suffix) {
        return parm(parmDate) + String.format(Locale.ROOT, "%06d", suffix);
    }

    /** {@code 1300-B-WRITE-TX}: the SYSTRAN record (merchant fields zero/spaces, both timestamps = now). */
    public static TransactionRecord transaction(String tranId, long acctId, BigDecimal interest, String cardNum,
                                                String timestamp) {
        return new TransactionRecord(tranId, INTEREST_TYPE_CD, INTEREST_CAT_CD, SOURCE,
                String.format(Locale.ROOT, "Int. for a/c %011d", acctId), interest, 0, "", "", "", cardNum,
                timestamp, timestamp);
    }

    /** {@code PARM-DATE PIC X(10)}. */
    static String parm(String value) {
        String v = value == null ? "" : value;
        return v.length() >= 10 ? v.substring(0, 10) : v + " ".repeat(10 - v.length());
    }

    /** {@code 1050-UPDATE-ACCOUNT}. */
    private void updateAccount(AccountRecord account, BigDecimal totalInterest) {
        BigDecimal zero = BigDecimal.ZERO.setScale(2);
        AccountRecord updated = new AccountRecord(account.acctId(), account.activeStatus(),
                CobolNumeric.truncate(account.currBal().add(totalInterest), 12, 2, true), account.creditLimit(),
                account.cashCreditLimit(), account.openDate(), account.expirationDate(), account.reissueDate(), zero,
                zero, account.addrZip(), account.groupId());
        io(() -> {
            if (!acctfile.rewrite(updated)) {
                throw new FileStatusException(ACCTFILE, "REWRITE", FileStatus.RECORD_NOT_FOUND);
            }
        }, "ERROR RE-WRITING ACCOUNT FILE");
    }

    /** {@code 1100-GET-ACCT-DATA}: INVALID KEY displays the key, then the status check abends. */
    private AccountRecord readAccount(long acctId) {
        Optional<AccountRecord> account;
        try {
            account = acctfile.read(acctId);
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING ACCOUNT FILE", e);
        }
        if (account.isEmpty()) {
            sysout.display(String.format(Locale.ROOT, "ACCOUNT NOT FOUND: %011d", acctId));
            throw sysout.ioAbend("ERROR READING ACCOUNT FILE",
                    new FileStatusException(ACCTFILE, "READ", FileStatus.RECORD_NOT_FOUND));
        }
        return account.get();
    }

    /** {@code 1110-GET-XREF-DATA}: {@code READ XREF-FILE KEY IS FD-XREF-ACCT-ID}. */
    private CardXrefRecord readXref(long acctId) {
        Optional<CardXrefRecord> xref;
        try {
            xref = xreffile.read(acctId);
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING XREF FILE", e);
        }
        if (xref.isEmpty()) {
            sysout.display(String.format(Locale.ROOT, "ACCOUNT NOT FOUND: %011d", acctId));
            throw sysout.ioAbend("ERROR READING XREF FILE",
                    new FileStatusException(XREFFILE, "READ", FileStatus.RECORD_NOT_FOUND));
        }
        return xref.get();
    }

    /** {@code 1200-GET-INTEREST-RATE} and {@code 1200-A-GET-DEFAULT-INT-RATE}. */
    private BigDecimal interestRate(String groupId, String tranTypeCd, int tranCatCd) {
        Optional<DisclosureGroupRecord> group;
        try {
            group = discgrp.read(new DisclosureGroupId(groupId == null ? "" : groupId, tranTypeCd, tranCatCd));
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING DISCLOSURE GROUP FILE", e);
        }
        if (group.isPresent()) {
            return group.get().intRate();
        }
        sysout.display("DISCLOSURE GROUP RECORD MISSING");
        sysout.display("TRY WITH DEFAULT GROUP CODE");
        Optional<DisclosureGroupRecord> fallback;
        try {
            fallback = discgrp.read(
                    new DisclosureGroupId(DisclosureGroupRepository.DEFAULT_GROUP, tranTypeCd, tranCatCd));
        } catch (FileStatusException e) {
            throw sysout.ioAbend("ERROR READING DEFAULT DISCLOSURE GROUP", e);
        }
        return fallback.orElseThrow(() -> sysout.ioAbend("ERROR READING DEFAULT DISCLOSURE GROUP",
                new FileStatusException(DISCGRP, "READ", FileStatus.RECORD_NOT_FOUND))).intRate();
    }

    private void writeTransaction(TransactionRecord transaction) {
        io(() -> transact.write(TransactionRecord.MAPPER.toRecord(transaction, encoding)),
                "ERROR WRITING TRANSACTION RECORD");
    }

    /**
     * ACCTFILE rewrites are durable when issued (VSAM), so a file-mode ACCTFILE keeps the accounts rewritten before the
     * abend; SYSTRAN is {@code DISP=(NEW,CATLG,DELETE)} and is only closed (the caller does not catalogue it).
     */
    private void flushOnAbend(RuntimeException abend) {
        for (Runnable close : new Runnable[] {tcatbalf::close, xreffile::close, discgrp::close, acctfile::close,
                transact::close}) {
            try {
                close.run();
            } catch (FileStatusException e) {
                if (e.status() != FileStatus.NOT_OPEN) {
                    abend.addSuppressed(e);
                }
            } catch (RuntimeException e) {
                abend.addSuppressed(e);
            }
        }
    }

    /** {@code 0100-XREFFILE-OPEN} displays the status right after its message. */
    private void open(Runnable operation, String message) {
        try {
            operation.run();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(message + e.status().code(), e);
        }
    }

    private void io(Runnable operation, String message) {
        try {
            operation.run();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(message, e);
        }
    }
}
