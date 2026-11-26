package com.carddemo.programs.coactupc.gateway;

import com.carddemo.programs.coactupc.copybook.AccountRecord;

/** ACCTDAT - account master VSAM KSDS. */
public interface AccountRepository {

    /** EXEC CICS READ DATASET('ACCTDAT') RIDFLD(acct-id X(11)). */
    FileResponse<AccountRecord> read(String acctIdKey);

    /** EXEC CICS READ FILE('ACCTDAT') UPDATE RIDFLD(acct-id X(11)). */
    FileResponse<AccountRecord> readForUpdate(String acctIdKey);

    /** EXEC CICS REWRITE FILE('ACCTDAT') FROM(300-byte record image). */
    FileResponse<Void> rewrite(String recordImage);
}
