package com.carddemo.programs.copaus0c.gateway;

import com.carddemo.programs.copaus0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus0c.copybook.PendingAuthSummary;

/** EXEC DLI access to the pending-authorization IMS database (PSB PSBPAUTB, PCB 1). */
public interface PendingAuthDbGateway {
    String PSB_NAME = "PSBPAUTB";

    /** EXEC DLI SCHD PSB((PSB-NAME)) NODHABEND - returns DIBSTAT. */
    String schedulePsb(String psbName);

    /** EXEC DLI TERM - returns DIBSTAT. */
    String terminate();

    /** EXEC DLI GU SEGMENT(PAUTSUM0) WHERE (ACCNTID = PA-ACCT-ID). */
    DliResult<PendingAuthSummary> getUniqueSummary(long paAcctId);

    /** EXEC DLI GNP SEGMENT(PAUTDTL1). */
    DliResult<PendingAuthDetails> getNextDetail();

    /** EXEC DLI GNP SEGMENT(PAUTDTL1) WHERE (PAUT9CTS = PA-AUTHORIZATION-KEY). */
    DliResult<PendingAuthDetails> getNextDetailByKey(String paAuthorizationKey);

    /** EXEC CICS SYNCPOINT (and implicit syncpoint at task end): commits and releases the scheduled PSB. */
    void syncpoint();
}
