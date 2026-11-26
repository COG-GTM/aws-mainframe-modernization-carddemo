package com.carddemo.programs.copaus0c.gateway;

import com.carddemo.programs.copaus0c.copybook.AccountRecord;

/** CICS READ DATASET('ACCTDAT ') keyed by account id. */
public interface AccountRepository {
    String DATASET = "ACCTDAT ";

    /** @param acctIdKey WS-CARD-RID-ACCT-ID-X PIC X(11) */
    FileReadResult<AccountRecord> read(String acctIdKey);
}
