package com.carddemo.batch.jobs.creastmt;

import com.carddemo.batch.io.KsdsFile;
import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.CardXrefRecord;
import com.carddemo.batch.model.CustomerRecord;
import com.carddemo.batch.model.StatementTransactionRecord;

import java.nio.file.Path;
import java.util.List;

/**
 * Replaces CBSTM03B, CBSTM03A's file-handler subprogram (its ALTER / GO TO dispatch on
 * DD name and operation). TRNXFILE and XREFFILE are read sequentially in key order;
 * CUSTFILE and ACCTFILE are read randomly by key. A failed keyed read is the COBOL
 * abend path (DISPLAY 'ERROR READING ...' + CEE3ABD).
 */
public class StatementFileRepository {

    private final KsdsFile<StatementTransactionRecord.TrnxKey, StatementTransactionRecord> trnxFile;
    private final KsdsFile<String, CardXrefRecord> xrefFile;
    private final KsdsFile<Long, CustomerRecord> custFile;
    private final KsdsFile<Long, AccountRecord> acctFile;

    public StatementFileRepository(Path trnx, Path xref, Path cust, Path acct) {
        this.trnxFile = KsdsFile.load(trnx, StatementTransactionRecord.CODEC, StatementTransactionRecord::trnxKey);
        this.xrefFile = KsdsFile.load(xref, CardXrefRecord.CODEC, CardXrefRecord::xrefCardNum);
        this.custFile = KsdsFile.load(cust, CustomerRecord.CODEC, CustomerRecord::custId);
        this.acctFile = KsdsFile.load(acct, AccountRecord.CODEC, AccountRecord::acctId);
    }

    /** TRNXFILE, sequential read in key order. */
    public List<StatementTransactionRecord> transactions() {
        return List.copyOf(trnxFile.values());
    }

    /** XREFFILE, sequential read in key order. */
    public List<CardXrefRecord> xrefs() {
        return List.copyOf(xrefFile.values());
    }

    /** CUSTFILE, keyed read. */
    public CustomerRecord customer(long custId) {
        return custFile.read(custId)
                .orElseThrow(() -> new IllegalStateException("ERROR READING CUSTFILE - RETURN CODE: 23 (key "
                        + Cobol.digits(custId, 9) + ")"));
    }

    /** ACCTFILE, keyed read. */
    public AccountRecord account(long acctId) {
        return acctFile.read(acctId)
                .orElseThrow(() -> new IllegalStateException("ERROR READING ACCTFILE - RETURN CODE: 23 (key "
                        + Cobol.digits(acctId, 11) + ")"));
    }
}
