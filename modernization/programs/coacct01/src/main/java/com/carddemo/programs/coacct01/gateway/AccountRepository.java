package com.carddemo.programs.coacct01.gateway;

import com.carddemo.programs.coacct01.copybook.AccountRecord;

/** EXEC CICS READ DATASET(ACCTDAT) RIDFLD(acct-id) INTO(ACCOUNT-RECORD) RESP RESP2. */
public interface AccountRepository {

    AccountReadResult read(String dataset, String ridfld);

    /** {@code record} is non-null only when {@code resp} is {@link CicsResp#NORMAL}. */
    record AccountReadResult(int resp, int resp2, AccountRecord record) {
    }
}
