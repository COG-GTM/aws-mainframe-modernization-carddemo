package com.carddemo.programs.cobil00c.gateway;

import com.carddemo.programs.cobil00c.copybook.AccountRecord;

/** CICS file ACCTDAT (VSAM KSDS keyed on ACCT-ID, 11 bytes). */
public interface AccountDataGateway {

    /** {@code EXEC CICS READ DATASET('ACCTDAT') RIDFLD(ACCT-ID) UPDATE}. */
    CicsResponse<AccountRecord> readForUpdate(String acctIdKey);

    /** {@code EXEC CICS REWRITE DATASET('ACCTDAT') FROM(ACCOUNT-RECORD)}. */
    CicsResponse<Void> rewrite(AccountRecord record);
}
