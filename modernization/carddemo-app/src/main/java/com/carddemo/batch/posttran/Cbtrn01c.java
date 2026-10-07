package com.carddemo.batch.posttran;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.print.ProgramCounts;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.FileStatusException;
import java.util.Locale;
import java.util.Optional;

/**
 * {@code CBTRN01C} (POSTTRAN STEP10): read-only pass over DALYTRAN. DISPLAYs each record, looks its card up in
 * XREFFILE and the account in ACCTFILE, and DISPLAYs the outcome. CUSTFILE, CARDFILE and TRANFILE are opened and
 * closed but never read. As in the program, the lookup also runs once after end of file, for the last record read
 * (spaces when DALYTRAN is empty). The DALYTRAN close error DISPLAYs {@code ERROR CLOSING CUSTOMER FILE}, as the
 * program does. RC 0.
 */
public final class Cbtrn01c {

    public static final String PROGRAM = "CBTRN01C";
    public static final String DALYTRAN = "DALYTRAN";
    public static final String CUSTFILE = "CUSTFILE";
    public static final String XREFFILE = "XREFFILE";
    public static final String CARDFILE = "CARDFILE";
    public static final String ACCTFILE = "ACCTFILE";
    public static final String TRANFILE = "TRANFILE";

    private final KsdsInput dalytran;
    private final KsdsInput custfile;
    private final KeyedDataset<String, CardXrefRecord> xreffile;
    private final KsdsInput cardfile;
    private final KeyedDataset<Long, AccountRecord> acctfile;
    private final KsdsInput tranfile;
    private final Sysout sysout;

    public Cbtrn01c(KsdsInput dalytran, KsdsInput custfile, KeyedDataset<String, CardXrefRecord> xreffile,
                    KsdsInput cardfile, KeyedDataset<Long, AccountRecord> acctfile, KsdsInput tranfile,
                    Sysout sysout) {
        this.dalytran = dalytran;
        this.custfile = custfile;
        this.xreffile = xreffile;
        this.cardfile = cardfile;
        this.acctfile = acctfile;
        this.tranfile = tranfile;
        this.sysout = sysout;
    }

    public ProgramCounts run() {
        sysout.display("START OF EXECUTION OF PROGRAM " + PROGRAM);
        io(dalytran::open, "ERROR OPENING DAILY TRANSACTION FILE");
        io(custfile::open, "ERROR OPENING CUSTOMER FILE");
        io(xreffile::open, "ERROR OPENING CROSS REF FILE");
        io(cardfile::open, "ERROR OPENING CARD FILE");
        io(acctfile::open, "ERROR OPENING ACCOUNT FILE");
        io(tranfile::open, "ERROR OPENING TRANSACTION FILE");
        String cardNum = " ".repeat(16);
        String tranId = " ".repeat(16);
        long read = 0;
        boolean endOfFile = false;
        while (!endOfFile) {
            Optional<FixedWidthRecord> next;
            try {
                next = dalytran.readNext();
            } catch (FileStatusException e) {
                throw sysout.ioAbend("ERROR READING DAILY TRANSACTION FILE", e);
            }
            if (next.isPresent()) {
                read++;
                FixedWidthRecord record = next.get();
                cardNum = record.getString("DALYTRAN-CARD-NUM");
                tranId = record.getString("DALYTRAN-ID");
                sysout.display(record.text());
            } else {
                endOfFile = true;
            }
            Optional<CardXrefRecord> xref = xreffile.read(cardNum.stripTrailing());
            if (xref.isPresent()) {
                CardXrefRecord x = xref.get();
                sysout.display("SUCCESSFUL READ OF XREF");
                sysout.display("CARD NUMBER: " + x.cardNum());
                sysout.display(String.format(Locale.ROOT, "ACCOUNT ID : %011d", x.acctId()));
                sysout.display(String.format(Locale.ROOT, "CUSTOMER ID: %09d", x.custId()));
                if (acctfile.read(x.acctId()).isPresent()) {
                    sysout.display("SUCCESSFUL READ OF ACCOUNT FILE");
                } else {
                    sysout.display("INVALID ACCOUNT NUMBER FOUND");
                    sysout.display(String.format(Locale.ROOT, "ACCOUNT %011d NOT FOUND", x.acctId()));
                }
            } else {
                sysout.display("INVALID CARD NUMBER FOR XREF");
                sysout.display("CARD NUMBER " + cardNum + " COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-" + tranId);
            }
        }
        io(dalytran::close, "ERROR CLOSING CUSTOMER FILE");
        io(custfile::close, "ERROR CLOSING CUSTOMER FILE");
        io(xreffile::close, "ERROR CLOSING CROSS REF FILE");
        io(cardfile::close, "ERROR CLOSING CARD FILE");
        io(acctfile::close, "ERROR CLOSING ACCOUNT FILE");
        io(tranfile::close, "ERROR CLOSING TRANSACTION FILE");
        sysout.display("END OF EXECUTION OF PROGRAM " + PROGRAM);
        return new ProgramCounts(read, 0);
    }

    private void io(Runnable operation, String message) {
        try {
            operation.run();
        } catch (FileStatusException e) {
            throw sysout.ioAbend(message, e);
        }
    }
}
