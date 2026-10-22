# Online transactions, bill pay and reports (Session 8)

Ports of the CICS programs COTRN00C, COTRN01C, COTRN02C, COBIL00C and CORPT00C onto the Session 6 framework (`../framework/`). Registered in `../areas/transaction.ts`, so menu options 6-10 of COMEN01C reach them.

| Program | TRANSID | Map / mapset | What it does |
|---|---|---|---|
| `cotrn00c.ts` | CT00 | COTRN0A / COTRN00 | Lists TRANSACT 10 rows per page (STARTBR/READNEXT/READPREV/ENDBR); PF7/PF8 page, ENTER restarts from the typed Tran ID (GTEQ), `S` on a row XCTLs to COTRN01C |
| `cotrn01c.ts` | CT01 | COTRN1A / COTRN01 | Shows one transaction (READ UPDATE, never rewritten); PF4 clear, PF5 list, PF3 back |
| `cotrn02c.ts` | CT02 | COTRN2A / COTRN02 | Adds a transaction: account (CXACAIX) or card (CCXREF) lookup, field edits, `CALL 'CSUTLDTC'` for both dates, next TRAN-ID = last key + 1, WRITE; PF5 copies the last record |
| `cobil00c.ts` | CB00 | COBIL0A / COBIL00 | Pays the full balance: WRITE a type-02/cat-2 TRANSACT record, REWRITE ACCTDAT with the balance reduced (decimal.js) |
| `corpt00c.ts` | CR00 | CORPT0A / CORPT00 | Monthly / yearly / custom report: builds the TRNRPT00 JCL and writes the 17 80-byte records to TDQ `JOBS` |

- `common.ts`: `ProgramCommarea` (COCOM01Y + the 58-byte `CDEMO-CT00/CT01/CT02/CB00-INFO` group, 218 bytes; CORPT00C uses the plain 160 bytes), COBOL MOVE helpers (`moveNumeric`, `moveAlnumToUnsigned`, `displayDigits`), `requireNormal` for commands coded without RESP.
- **Job submission sink**: `createCORPT00C({ jobSink })` takes a `JobSubmissionSink` `(ctx, queue, record) => { resp, resp2 }`. The registered `CORPT00C` uses `tdqJobSink`, i.e. `EXEC CICS WRITEQ TD QUEUE('JOBS')`, so submitted JCL is in `runtime.tdQueues.get('JOBS')`. A non-NORMAL RESP gives "Unable to Write TDQ (JOBS)...".
- Tests: `test/online/transaction/` (driven through signon + COMEN01C). There is no TRANSACT seed, so the tests load the 300 DALYTRAN records (`loadSeedCatalog({ transactFromDailytran: true })`).

## Faithful COBOL quirks (kept on purpose)
- COTRN00C: 300 records = 30 pages; page 30 (exactly 10 rows) shows "You have reached the bottom of the page..." because the look-ahead READNEXT hits ENDFILE. PF7 always sets NEXT-PAGE-FLG = 'Y'. ENTER without a Tran ID restarts at page 1 (TRNIDIN is resent as spaces). Selecting an empty row does nothing. With an **empty TRANSACT** the STARTBR NOTFND is followed by an unconditional `ENDBR` (no RESP) that raises INVREQ, so the task abends AEIP, as it would in CICS.
- COTRN01C / COTRN02C / COBIL00C account and tran-id inputs have no JUSTIFY: a short id is space-padded, so COTRN02C rejects it as not numeric and COTRN01C does not find it.
- COTRN02C: amount must be typed as `+99999999.99`/`-99999999.99`; dates go through CSUTLDTC and message 2513 (unsupported range) is accepted. PF5 on the seed data copies a record whose TRAN-PROC-TS is blank, so it stops at "Proc Date can NOT be empty...". The new record's FILLER keeps the bytes of the record read by READPREV (INITIALIZE skips FILLER).
- COBIL00C: the payment is `MOVE ACCT-CURR-BAL (S9(10)V99) TO TRAN-AMT (S9(9)V99)`, so a balance >= 1,000,000,000 is high-order truncated and the account keeps the excess. A CXACAIX / WRITE failure does not stop the flow: the account is still REWRITten (the last message sent wins). The confirmation `N` clears the screen.
- CORPT00C: custom date parts are NUMVAL-C'd into PIC 99 / 9999 and moved back to the screen (`1` -> `01`; invalid text -> `00`, then caught by CSUTLDTC). Every SEND ends the task (GO TO RETURN-TO-CICS).

## LIMITATIONS
- CICS programs cannot run under GnuCOBOL, so there are no golden files: expected values were derived by tracing the COBOL source.
- Fresh WORKING-STORAGE items without VALUE (TRAN-RECORD, ACCOUNT-RECORD, CARD-XREF-RECORD, CARDDEMO-COMMAREA on EIBCALEN = 0) are modelled as LOW-VALUES; on z/OS their content is whatever the initial WS image holds.
- `MOVE X(n) TO 9(n)` (COBIL00C ACCT-ID, COTRN02C TRAN-CAT-CD / TRAN-MERCHANT-ID, the TRAN-ID increment) assumes Enterprise COBOL's unsigned-zone fix-up of the last byte (`OI X'F0'`): a trailing space becomes `0`, e.g. bill pay for `0000000001 ` reads account `00000000010`. Not verified on a mainframe.
- STARTBR with HIGH-VALUES on an empty TRANSACT returns NORMAL in the foundation's browse model (then READPREV gives ENDFILE and the next TRAN-ID is 1); real VSAM may return NOTFND ("Transaction ID NOT found...").
- `DISPLAY 'RESP:' ...` / `DISPLAY 'PROCESS ENTER KEY'` (CICS job log output) are not reproduced.
- Screen behaviour (SEND without ERASE, FSET retransmission) follows the framework model; see `../README.md`.
- Shared-helper note for the orchestrator: `ProgramCommarea` (COCOM01Y + program-specific tail) and `requireNormal` are local to this module; COCRDLIC-style programs in other areas may want the same thing upstreamed into the framework.
