package com.carddemo.programs.copaus1c.gateway;

import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus1c.copybook.PendingAuthSummary;

/** EXEC DLI calls COPAUS1C issues against the pending-authorization database (PSB PSBPAUTB, PCB 1). */
public interface PendingAuthDbGateway {

    /** EXEC DLI SCHD PSB(psbName) NODHABEND. Returns DIBSTAT. */
    String schedulePsb(String psbName);

    /** EXEC DLI TERM. Returns DIBSTAT. */
    String terminate();

    /** EXEC DLI GU SEGMENT(PAUTSUM0) WHERE(ACCNTID = acctId). */
    DliResult<PendingAuthSummary> getUniqueSummary(long acctId);

    /** EXEC DLI GNP SEGMENT(PAUTDTL1) WHERE(PAUT9CTS = key). */
    DliResult<PendingAuthDetails> getNextDetailWhere(AuthorizationKey key);

    /** EXEC DLI GNP SEGMENT(PAUTDTL1) (unqualified, next child under current parent). */
    DliResult<PendingAuthDetails> getNextDetail();

    /** EXEC DLI REPL SEGMENT(PAUTDTL1) FROM(details). Returns DIBSTAT. */
    String replaceDetail(PendingAuthDetails details);
}
