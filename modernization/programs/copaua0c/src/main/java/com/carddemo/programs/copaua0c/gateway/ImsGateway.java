package com.carddemo.programs.copaua0c.gateway;

import com.carddemo.programs.copaua0c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaua0c.copybook.PendingAuthSummary;

/** EXEC DLI calls against PSB PSBPAUTB (PAUTSUM0 root / PAUTDTL1 child). Methods return DIBSTAT. */
public interface ImsGateway {

    /** {@code segment} is null unless the status is OK. */
    record GetResult(String status, PendingAuthSummary segment) {
    }

    /** EXEC DLI SCHD PSB((psbName)) NODHABEND. */
    String schedule(String psbName);

    /** EXEC DLI TERM. */
    String terminate();

    /** EXEC DLI GU SEGMENT(PAUTSUM0) WHERE(ACCNTID = acctId). */
    GetResult getUniqueSummary(long acctId);

    /** EXEC DLI REPL SEGMENT(PAUTSUM0) FROM(PENDING-AUTH-SUMMARY). */
    String replaceSummary(PendingAuthSummary summary);

    /** EXEC DLI ISRT SEGMENT(PAUTSUM0) FROM(PENDING-AUTH-SUMMARY). */
    String insertSummary(PendingAuthSummary summary);

    /** EXEC DLI ISRT SEGMENT(PAUTSUM0) WHERE(ACCNTID = acctId) SEGMENT(PAUTDTL1) FROM(PENDING-AUTH-DETAILS). */
    String insertDetail(long acctId, PendingAuthDetails details);
}
