package com.carddemo.programs.cotrn00c;

/**
 * CICS file control on dataset TRANSACT (VSAM KSDS keyed by TRAN-ID X(16)), browse commands only.
 * Implementations must follow CICS browse semantics:
 * <ul>
 *   <li>{@code startBr}: generic GTEQ positioning (COTRN00C comments GTEQ out, which is the default);
 *       NOTFND when no key is &gt;= ridfld; a key of HIGH-VALUES positions at end of file.</li>
 *   <li>{@code readNext}/{@code readPrev}: ENDFILE past either end; INVREQ when no browse is active;
 *       READPREV straight after STARTBR on a key that does not exist returns NOTFND.</li>
 *   <li>{@code endBr}: INVREQ when no browse is active.</li>
 * </ul>
 */
public interface TransactFileGateway {

    CicsResponse startBr(String ridfld);

    ReadResult readNext();

    ReadResult readPrev();

    CicsResponse endBr();
}
