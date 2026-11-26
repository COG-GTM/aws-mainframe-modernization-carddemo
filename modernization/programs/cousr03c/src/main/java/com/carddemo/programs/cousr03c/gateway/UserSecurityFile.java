package com.carddemo.programs.cousr03c.gateway;

import com.carddemo.programs.cousr03c.copybook.SecUserData;

/** Gateway for CICS file control on dataset USRSEC (VSAM KSDS of {@link SecUserData}, key SEC-USR-ID X(08)). */
public interface UserSecurityFile {

    /** Result of EXEC CICS READ ... INTO(SEC-USER-DATA); {@code record} is null unless resp is NORMAL. */
    record ReadResult(CicsResp resp, SecUserData record) {
    }

    /** EXEC CICS READ DATASET('USRSEC') RIDFLD(key) KEYLENGTH(8) UPDATE. */
    ReadResult readForUpdate(String key);

    /** EXEC CICS DELETE DATASET('USRSEC') with no RIDFLD: deletes the record held by the preceding READ UPDATE. */
    CicsResp deleteHeldRecord();

    /** End of task (CICS RETURN / XCTL end of unit of work): releases any READ UPDATE hold. */
    default void endOfTask() {
    }
}
