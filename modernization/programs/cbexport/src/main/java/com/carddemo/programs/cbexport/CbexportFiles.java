package com.carddemo.programs.cbexport;

import com.carddemo.programs.cbexport.io.RecordReader;
import com.carddemo.programs.cbexport.io.RecordWriter;

/**
 * The six files CBEXPORT opens, named after their JCL DD statements (app/jcl/CBEXPORT.jcl).
 *
 * @param custFile  CUSTFILE -> CUSTOMER-INPUT (AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS, CVCUS01Y, 500 bytes)
 * @param acctFile  ACCTFILE -> ACCOUNT-INPUT (AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS, CVACT01Y, 300 bytes)
 * @param xrefFile  XREFFILE -> XREF-INPUT (AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS, CVACT03Y, 50 bytes)
 * @param transact  TRANSACT -> TRANSACTION-INPUT (AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS, CVTRA05Y, 350 bytes)
 * @param cardFile  CARDFILE -> CARD-INPUT (AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS, CVACT02Y, 150 bytes)
 * @param expFile   EXPFILE -> EXPORT-OUTPUT (AWS.M2.CARDDEMO.EXPORT.DATA, CVEXPORT, 500 bytes)
 */
public record CbexportFiles(
        RecordReader custFile,
        RecordReader acctFile,
        RecordReader xrefFile,
        RecordReader transact,
        RecordReader cardFile,
        RecordWriter expFile) {

    /** Closes every file, ignoring errors (used after an abend, where 6000-FINALIZE never runs). */
    public void closeAll() {
        custFile.close();
        acctFile.close();
        xrefFile.close();
        transact.close();
        cardFile.close();
        expFile.close();
    }
}
