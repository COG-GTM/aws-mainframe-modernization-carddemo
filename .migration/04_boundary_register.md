# 04 - Boundary register (module-wide, append-only)

Owned by `!mf_migration_setup`; appended by `!mf_module_inventory_analysis` (register mode, first pass), `!mf_stream_analysis` (register mode, full contract), `!mf_stream_migration_plan` (decide mode) and `!mf_program_migration` (implement mode). **Never rewrite or delete an entry**: append a dated update row in the entry's history instead. Taxonomy and procedure: `!mf_boundary_resolution`.

## Schema

Every entry is a level-3 section `### BND-<NNN> <short title>` with this table, then a `History` list (append-only, dated).

| Field | Content |
|---|---|
| Class | exactly one of B1 intra-stream call, B2 shared module, B3 foreign subroutine, B4 data-access leaf, B5 cross-transaction switch, B6 queue/MQ/event, B7 dataset hand-off, B8 online-to-batch, B9 scheduler dependency, B10 shared data contract, B11 external system |
| Why this class | one line |
| Streams affected | stream ids from the inventory |
| Caller / producer + cite | `<path>:<line>` |
| Callee / consumer / resource | name; present in repo? (Y/N) |
| Interface | copybook / record layout / DD / queue / table; fields in/out with PIC; return-code / status protocol incl. every value the caller branches on |
| Read-only or state-changing | which datasets/tables/segments |
| Transactional coupling | inside caller's unit of work? commits? partial-failure behavior |
| Volume / frequency / latency | |
| Error, timeout, retry semantics | incl. abend behavior |
| Idempotency | is retry safe, and why |
| Security context | user id / terminal / region assumed |
| Owner today / who to ask | |
| Strangler direction + facade | outbound / inbound / both; named routing point |
| Required action type | e.g. decide-at-plan, port-once, keep-file-contract, coexistence-decision, lead-time-request |
| Contract complete? | YES / NO (+ what is unresolved: a NO is a STOP C blocker) |
| Status | UNDECIDED -> DECIDED -> IMPLEMENTED (or DEFERRED with dependency, impact, re-entry condition) |
| Decision / owner / request status / seam / verification | filled in decide mode |
| PR / tests / residual risk | filled in implement mode |

## Counts by class (update when appending)

| Class | Count |
|---|---|
| B2 | 1 |
| B3 | 1 |
| B4 | 2 |
| B5 | 1 |
| B6 | 2 |
| B7 | 2 |
| B8 | 2 |
| B9 | 1 |
| B10 | 4 |
| B11 | 2 |
| **Total** | **18** |

(2026-11-19: counts set by the first pass; later playbooks append a dated recount line here.)

## Entries


## First pass - `!mf_module_inventory_analysis`, 2026-11-19 (session devin-c4fce2ffab2c4abaac2aa02e217e00e4)

Register mode only: no decisions. Fields not listed (volume, retry, security, owner, strangler facade) are completed by `!mf_stream_analysis` for the chosen stream; until then each entry is `Contract complete? NO` unless stated.

### BND-001 LE runtime services (CEE3ABD, CEEDAYS)
- **Class:** B11 - runtime outside the module
- **Streams affected:** all batch (abend), ONL-M02/M08/M09 (CEEDAYS)
- **Caller / producer + cite:** app/cbl/CBTRN02C.cbl:711; app/cbl/CSUTLDTC.cbl:116
- **Callee present in repo:** N (IBM LE)
- **Interface / status protocol:** CEE3ABD(ABCODE S9(9) BINARY=999, TIMING) never returns
- **Read-only or state-changing:** abend = step failure; target RC 16
- **Contract complete?** YES for POSTTRAN
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-002 DALYTRAN.PS inbound daily file
- **Class:** B7 - dataset produced outside the module
- **Streams affected:** BAT-POSTTRAN (and orphan CBTRN01C)
- **Caller / producer + cite:** app/jcl/POSTTRAN.jcl:30-31; app/cbl/CBTRN02C.cbl:29-32,346
- **Callee present in repo:** producer absent from repo
- **Interface / status protocol:** CVTRA06Y 350-byte FB; FILE STATUS 00/10 else abend (:345-367)
- **Read-only or state-changing:** read-only
- **Contract complete?** NO: producer, delivery time, encoding (ASCII vs CP037) unknown
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-003 DALYREJS(+1) outbound rejects GDG
- **Class:** B7 - dataset for consumers outside the stream
- **Streams affected:** BAT-POSTTRAN
- **Caller / producer + cite:** app/jcl/POSTTRAN.jcl:34-38; app/cbl/CBTRN02C.cbl:446-465; app/jcl/DALYREJS.jcl:24-25
- **Callee present in repo:** no consumer in repo
- **Interface / status protocol:** 430 bytes = DALYTRAN 350 + reason 9(04) + desc X(76)
- **Read-only or state-changing:** state-changing (new generation)
- **Contract complete?** NO: consumer unknown
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-004 TRANSACT KSDS shared contract
- **Class:** B10 - written by POSTTRAN and by online/batch programs outside it
- **Streams affected:** BAT-POSTTRAN, ONL-M06/M07/M08/M09/M10, BAT-TRANREPT, BAT-CREASTMT, BAT-CBEXPORT, TRANBKP/COMBTRAN/TRANIDX
- **Caller / producer + cite:** app/cbl/CBTRN02C.cbl:256,564; app/cbl/COBIL00C.cbl:512; app/cbl/COTRN02C.cbl:713; app/jcl/COMBTRAN.jcl:43-48; app/jcl/TRANBKP.jcl:23-66
- **Callee present in repo:** Y
- **Interface / status protocol:** CVTRA05Y 350, key TRAN-ID X(16), AIX PROC-TS (app/jcl/TRANFILE.jcl:84); status 00 else abend; 22 on duplicate
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO: single writer, TRAN-ID allocation (online last+1 COBIL00C.cbl:212-219 vs feed IDs), OPEN OUTPUT semantics (OQ-1)
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-005 ACCTDATA KSDS shared contract
- **Class:** B10 - rewritten by POSTTRAN, INTCALC and online bill pay/account update
- **Streams affected:** BAT-POSTTRAN, BAT-INTCALC, ONL-M01/M02/M10, BAT-READACCT, BAT-CBEXPORT, BAT-CREASTMT
- **Caller / producer + cite:** app/cbl/CBTRN02C.cbl:311,395,554; app/cbl/CBACT04C.cbl:356; app/cbl/COBIL00C.cbl:379
- **Callee present in repo:** Y
- **Interface / status protocol:** CVACT01Y 300, key 9(11); REWRITE INVALID KEY -> 109 (:555-558)
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO: write ownership during coexistence, @Version policy
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-006 TCATBALF KSDS shared contract
- **Class:** B10 - written by POSTTRAN, read by INTCALC/PRTCATBL
- **Streams affected:** BAT-POSTTRAN, BAT-INTCALC, PRTCATBL, TCATBALF rebuild job
- **Caller / producer + cite:** app/cbl/CBTRN02C.cbl:329,474,510,528; app/jcl/INTCALC.jcl:28; app/jcl/PRTCATBL.jcl:33
- **Callee present in repo:** Y
- **Interface / status protocol:** CVTRA01Y 50, key acct 9(11)+type X(2)+cat 9(4); READ 23 -> create
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO: single writer confirmation
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-007 CARDXREF KSDS shared contract
- **Class:** B10 - read by POSTTRAN, maintained elsewhere
- **Streams affected:** BAT-POSTTRAN, BAT-INTCALC, BAT-TRANREPT, BAT-CREASTMT, online card/tran programs
- **Caller / producer + cite:** app/cbl/CBTRN02C.cbl:275,383; app/jcl/XREFFILE.jcl:39-62
- **Callee present in repo:** Y
- **Interface / status protocol:** CVACT03Y 50, key card X(16); INVALID KEY -> reject 100
- **Read-only or state-changing:** read-only for POSTTRAN
- **Contract complete?** NO: writer/refresh owner
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-008 Scheduler chain around POSTTRAN
- **Class:** B9 - job dependencies and COND/trigger semantics
- **Streams affected:** BAT-POSTTRAN, CLOSEFIL, CBPAUP0J, WAITSTEP, OPENFIL, TRANBKP, TRANIDX
- **Caller / producer + cite:** app/scheduler/CardDemo.ca7:18-124; scripts/run_posting.sh
- **Callee present in repo:** Y (CA-7 listing); absent from Control-M
- **Interface / status protocol:** completion signal = RC (0/4 ok, abend fails)
- **Read-only or state-changing:** n/a
- **Contract complete?** NO: who schedules during coexistence; RC 4 handling downstream
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-009 Online CICS co-writers while batch runs
- **Class:** B8 - online updates TRANSACT/ACCTDAT that POSTTRAN also updates
- **Streams affected:** BAT-POSTTRAN, ONL-M08, ONL-M10
- **Caller / producer + cite:** app/jcl/CLOSEFIL.jcl:22-26; app/cbl/COBIL00C.cbl:379,512; app/cbl/COTRN02C.cbl:713
- **Callee present in repo:** Y
- **Interface / status protocol:** CEMT SET FIL CLO/OPE (TRANSACT, CCXREF, ACCTDAT, CXACAIX, USRSEC)
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO: coexistence window, locking
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-010 CORPT00C submits TRANREPT via TDQ JOBS
- **Class:** B8 - online transaction submits a batch job
- **Streams affected:** ONL-M09 -> BAT-TRANREPT
- **Caller / producer + cite:** app/cbl/CORPT00C.cbl:94,515-518; app/csd/CARDDEMO.CSD:499-503
- **Callee present in repo:** Y
- **Interface / status protocol:** 80-byte JCL card images to INREADER
- **Read-only or state-changing:** state-changing (job submission)
- **Contract complete?** NO (stream analysis)
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-011 Navigation hub / XCTL returns
- **Class:** B5 - every online stream XCTLs to COSGN00C/COMEN01C/COADM01C or CDEMO-FROM-PROGRAM
- **Streams affected:** all ONL-*
- **Caller / producer + cite:** app/cbl/COACTVWC.cbl:336-349; app/cpy/COCOM01Y.cpy
- **Callee present in repo:** Y
- **Interface / status protocol:** CARDDEMO-COMMAREA
- **Read-only or state-changing:** n/a
- **Contract complete?** NO (online target state N/A)
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-012 MQ authorization request/reply (CP00)
- **Class:** B6 - MQGET/MQPUT1
- **Streams affected:** ONL-CP00
- **Caller / producer + cite:** app/app-authorization-ims-db2-mq/cbl/COPAUA0C.cbl:262,400,758,956
- **Callee present in repo:** queue defs absent
- **Interface / status protocol:** CMQ* copybooks
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-013 MQ request/response CDRA/CDRD
- **Class:** B6 - MQ put/get
- **Streams affected:** ONL-CDRA, ONL-CDRD
- **Caller / producer + cite:** app/app-vsam-mq/cbl/COACCT01.cbl:233-602; app/app-vsam-mq/cbl/CODATE01.cbl:182-506; app/app-vsam-mq/README.md:53-54
- **Callee present in repo:** README only
- **Interface / status protocol:** CARDDEMO.REQUEST/RESPONSE.QUEUE
- **Read-only or state-changing:** read-only
- **Contract complete?** NO
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-014 IMS pending-authorization DB (DL/I)
- **Class:** B4 - EXEC DLI / CBLTDLI physical IMS layer
- **Streams affected:** ONL-M11, ONL-CP00, BAT-CBPAUP0J, BAT-LOADPADB, BAT-UNLDPADB, BAT-UNLDGSAM
- **Caller / producer + cite:** app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl:223-355; COPAUS1C.cbl:439-525; ims/PSBPAUTB.psb
- **Callee present in repo:** Y (DBD/PSB in repo)
- **Interface / status protocol:** PCB status codes
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-015 DB2 tables (AUTHFRDS, TRNTYPE, TRNTYCAT)
- **Class:** B4 - EXEC SQL physical DB2 layer
- **Streams affected:** ONL-M11 (COPAUS2C), ONL-A05/A06, BAT-MNTTRDB2
- **Caller / producer + cite:** app/app-authorization-ims-db2-mq/cbl/COPAUS2C.cbl:141,222; app/app-transaction-type-db2/ddl/*.ddl
- **Callee present in repo:** Y (DDL/DCLGEN)
- **Interface / status protocol:** SQLCODE; DSNTIAC
- **Read-only or state-changing:** state-changing
- **Contract complete?** NO
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-016 Shared programs (port once)
- **Class:** B2 - executed by more than one stream
- **Streams affected:** see inventory §7
- **Caller / producer + cite:** CSUTLDTC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRTUPC, COUSR02C, COUSR03C
- **Callee present in repo:** Y
- **Interface / status protocol:** XCTL+COMMAREA / CALL USING
- **Read-only or state-changing:** mixed
- **Contract complete?** YES (owners in inventory §7)
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-017 Absent programs
- **Class:** B3 - referenced, source absent
- **Streams affected:** ONL-CDV1; CBADMCDJ defs
- **Caller / producer + cite:** app/csd/CARDDEMO.CSD:388-390; app/jcl/CBADMCDJ.jcl:98-157
- **Callee present in repo:** N
- **Interface / status protocol:** unknown
- **Read-only or state-changing:** unknown
- **Contract complete?** NO: obtain source or declare obsolete
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

### BND-018 Print / transfer externals (TXT2PDF, FTP)
- **Class:** B11 - external utilities consuming module output
- **Streams affected:** BAT-CREASTMT chain
- **Caller / producer + cite:** app/jcl/TXT2PDF1.JCL:24-37; app/jcl/FTPJCL.JCL:30
- **Callee present in repo:** N
- **Interface / status protocol:** TSO REXX / FTP
- **Read-only or state-changing:** n/a
- **Contract complete?** NO
- **Status:** UNDECIDED
- **History:** 2026-11-19 registered (first pass)

## Stream analysis - POSTTRAN, `!mf_stream_analysis`, 2026-11-19 (session devin-c4fce2ffab2c4abaac2aa02e217e00e4)

Contracts completed for the boundaries this stream touches (detail in `functional/CARDDEMO/POSTTRAN_analysis.md` §5). No new crossings found (no B1-B6 in CBTRN02C). Status stays UNDECIDED until the plan.

### BND-001 update (stream analysis)
- **Contract complete?** YES. Only CBTRN02C abend path in this stream: 25 error branches PERFORM 9999-ABEND-PROGRAM (`app/cbl/CBTRN02C.cbl:247-689`), each preceded by a verbatim `ERROR ...` line and `FILE STATUS IS: NNNN<xxxx>` (`:715-727`). Volume: 0 per normal run. Retry: none. Idempotency: n/a. Security: none. Owner: IBM LE (runtime).
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-002 update (stream analysis)
- **Contract complete?** NO (producer). Contract: FB 350, CVTRA06Y, sequential, read to EOF (`:345-369`), status 00/10 else abend; empty file valid (0 processed, RC 0). Volume: 300 records in the repo sample (`app/data/ASCII/dailytran.txt`), daily. Encoding of the production feed unknown (OQ-5). DALYTRAN-PROC-TS (CVTRA06Y:17) is never used except inside the reject image. Owner: unknown.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-003 update (stream analysis)
- **Contract complete?** NO (consumer). Contract: FB 430 = input image 350 + reason 9(04) + description X(76) (`:176-182`), one record per reject in input order; empty generation when no rejects; DCB from `POSTTRAN.jcl:34-38` (catalogued GDG via `app/jcl/DALYREJS.jcl:24-25`). Owner: unknown.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-004 update (stream analysis)
- **Contract complete?** YES. POSTTRAN inserts one row per posted record; key TRAN-ID X(16) from the feed; duplicate -> status 22 -> abend (R-06). Other writers: online COBIL00C (`app/cbl/COBIL00C.cbl:212-219,512`) and COTRN02C (`app/cbl/COTRN02C.cbl:444-451,713`) allocate last-ID+1 by browsing to HIGH-VALUES; TRANBKP/COMBTRAN rebuild the file. Transactional coupling: same unit as BND-005/006 (D-009). Owner: transaction context.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-005 update (stream analysis)
- **Contract complete?** YES. POSTTRAN reads by ACCT-ID (`:395`) and rewrites CURR-BAL, CYC-CREDIT, CYC-DEBIT (`:545-554`). Other writers COBIL00C (`:379`), COACTUPC, CBACT04C (`app/cbl/CBACT04C.cbl:356`). Concurrency: CICS files closed during the chain (BND-009). Owner: account context.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-006 update (stream analysis)
- **Contract complete?** YES. Read (`:474`), status 23 -> create with balance = amount (`:503-510`), else rewrite balance + amount (`:526-528`). Read by CBACT04C (INTCALC). Owner: reference context.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-007 update (stream analysis)
- **Contract complete?** YES. Read-only by card number (`:383`); 50 rows in the sample. Maintained by XREFFILE load and CBIMPORT. Owner: card context.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-008 update (stream analysis)
- **Contract complete?** YES for the Java side: completion = exit code (0/4 success, 16 failure). Predecessor CBPAUP0J, successor WAITSTEP (`app/scheduler/CardDemo.ca7:68-79`); operator script `scripts/run_posting.sh` brackets with TRANBKP and TRANIDX. Owner of the replacement scheduler: unknown.
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)

### BND-009 update (stream analysis)
- **Contract complete?** YES. CLOSEFIL closes TRANSACT, CCXREF, ACCTDAT, CXACAIX, USRSEC in CICS (`app/jcl/CLOSEFIL.jcl:22-30`); OPENFIL reopens. Online writers excluded from the stream (D-010).
- **Status:** UNDECIDED
- **History:** 2026-11-19 contract completed by `!mf_stream_analysis` (POSTTRAN)
