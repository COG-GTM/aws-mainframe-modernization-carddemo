package com.carddemo.programs.cousr00c;

/**
 * Gateway for the CICS file USRSEC (VSAM KSDS of SEC-USER-DATA keyed by SEC-USR-ID, KEYLENGTH 8).
 * Mirrors the browse commands COUSR00C issues; results are reported via RESP, never thrown.
 */
public interface UserSecurityFile {

    String DATASET = "USRSEC  ";

    /** EXEC CICS STARTBR DATASET(USRSEC) RIDFLD(key) KEYLENGTH(8) - GTEQ positioning (the CICS default). */
    CicsResponse startBr(String ridfld);

    /** EXEC CICS READNEXT INTO(SEC-USER-DATA). */
    BrowseRead readNext();

    /** EXEC CICS READPREV INTO(SEC-USER-DATA). */
    BrowseRead readPrev();

    /** EXEC CICS ENDBR - COUSR00C codes no RESP here, so a non-NORMAL result abends the task. */
    CicsResponse endBr();
}
