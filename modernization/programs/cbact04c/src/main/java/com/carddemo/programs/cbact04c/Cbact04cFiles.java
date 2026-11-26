package com.carddemo.programs.cbact04c;

import com.carddemo.programs.cbact04c.copybook.AccountRecord;
import com.carddemo.programs.cbact04c.copybook.CardXrefRecord;
import com.carddemo.programs.cbact04c.copybook.DisGroupRecord;
import com.carddemo.programs.cbact04c.copybook.TranCatBalRecord;
import com.carddemo.programs.cbact04c.copybook.TranRecord;
import com.carddemo.programs.cbact04c.io.IndexedFile;
import com.carddemo.programs.cbact04c.io.SequentialInputFile;
import com.carddemo.programs.cbact04c.io.SequentialOutputFile;

/**
 * The five DDs of INTCALC.jcl STEP15.
 *
 * @param tcatbalFile  TCATBALF - TCATBALF.VSAM.KSDS, read sequentially in key order
 * @param xrefFile     XREFFILE/XREFFIL1 - CARDXREF KSDS read by alternate key XREF-ACCT-ID
 * @param discgrpFile  DISCGRP - DISCGRP.VSAM.KSDS keyed by DIS-GROUP-KEY
 * @param accountFile  ACCTFILE - ACCTDATA.VSAM.KSDS keyed by ACCT-ID, opened I-O
 * @param transactFile TRANSACT - SYSTRAN(+1), new RECFM=F LRECL=350
 */
public record Cbact04cFiles(
        SequentialInputFile<TranCatBalRecord> tcatbalFile,
        IndexedFile<Long, CardXrefRecord> xrefFile,
        IndexedFile<DisGroupRecord.Key, DisGroupRecord> discgrpFile,
        IndexedFile<Long, AccountRecord> accountFile,
        SequentialOutputFile<TranRecord> transactFile) {
}
