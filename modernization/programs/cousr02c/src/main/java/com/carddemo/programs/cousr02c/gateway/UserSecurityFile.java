package com.carddemo.programs.cousr02c.gateway;

import com.carddemo.programs.cousr02c.copybook.SecUserData;

/** Gateway for the USRSEC VSAM KSDS (key SEC-USR-ID, 8 bytes; record CSUSR01Y, 80 bytes) as used by COUSR02C. */
public interface UserSecurityFile {

    /** {@code EXEC CICS READ DATASET(dataset) RIDFLD(key) KEYLENGTH(8) UPDATE}. */
    ReadResult readForUpdate(String dataset, String key);

    /** {@code EXEC CICS REWRITE DATASET(dataset) FROM(record)} of the record held by the preceding READ UPDATE. */
    CicsResult rewrite(String dataset, SecUserData record);

    /** End of task (CICS RETURN / XCTL): any record held for update is released. */
    void endTask();
}
