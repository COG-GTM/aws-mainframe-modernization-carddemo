package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.io.ExportRecordReader;
import com.carddemo.programs.cbimport.io.RecordWriter;

/** FILE-CONTROL: the program's files keyed by their JCL DD names. */
public record CbimportFiles(
        ExportRecordReader expfile,     // EXPORT-INPUT       DD EXPFILE
        RecordWriter custout,           // CUSTOMER-OUTPUT    DD CUSTOUT
        RecordWriter acctout,           // ACCOUNT-OUTPUT     DD ACCTOUT
        RecordWriter xrefout,           // XREF-OUTPUT        DD XREFOUT
        RecordWriter trnxout,           // TRANSACTION-OUTPUT DD TRNXOUT
        RecordWriter cardout,           // CARD-OUTPUT        DD CARDOUT
        RecordWriter errout) {          // ERROR-OUTPUT       DD ERROUT
}
