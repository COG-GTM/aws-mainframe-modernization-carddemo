# CardDemo → AWS migration inventory

Status: **v1 (Discovery session)**. Source of truth for every artifact in the repository and its AWS target.
Every row cites the source file(s) it was derived from (paths relative to repo root). Contracts referenced:
[`contracts/data-model.md`](contracts/data-model.md), [`contracts/api.md`](contracts/api.md),
[`contracts/batch.md`](contracts/batch.md), [`contracts/messaging.md`](contracts/messaging.md),
[`contracts/conventions.md`](contracts/conventions.md).

LOC = non-blank, non-comment source lines (column 7 ≠ `*`); physical line counts in parentheses.
Complexity: **L** < 300 LOC and simple CRUD; **M** 300–800 LOC or browse/multi-file logic; **H** > 800 LOC,
multi-file update with locking, or IMS/DB2/MQ integration.

Owning sessions (short codes used below):

| Code | Session | Directory |
|---|---|---|
| S1 | Discovery & contracts (this PR) | `aws/contracts/`, `aws/migration-inventory.md` |
| DM | Data migration | `aws/db/`, `aws/etl/` |
| ON | Online services (Spring Boot) | `aws/services/` |
| BA | Batch (Spring Batch + Step Functions) | `aws/batch/` |
| FE | Frontend (React + TS + Vite) | `aws/frontend/` |
| IN | Infra (IaC) | `aws/infra/` |
| VA | Validation | `aws/validation/` |

## 0. Naming corrections (vs. orchestrator brief)

Verified against `app/csd/CARDDEMO.CSD`, `app/cpy/COMEN02Y.cpy`, `app/cpy/COADM02Y.cpy` and `app/cbl/`:

| Brief / assumption | Actual source name | Evidence |
|---|---|---|
| card programs | **`COCRDLIC`** (list, `CCLI`), **`COCRDSLC`** (detail, `CCDL`), **`COCRDUPC`** (update, `CCUP`) | `app/csd/CARDDEMO.CSD`, `app/cpy/COMEN02Y.cpy` options 3–5 |
| user admin programs | **`COUSR00C`** (list `CU00`), **`COUSR01C`** (add `CU01`), **`COUSR02C`** (update `CU02`), **`COUSR03C`** (delete `CU03`) | `app/csd/CARDDEMO.CSD`, `app/cpy/COADM02Y.cpy` options 1–4 |
| bill payment | **`COBIL00C`** (`CB00`) | `app/csd/CARDDEMO.CSD`, `COMEN02Y` option 10 |
| reports | **`CORPT00C`** (`CR00`) | `app/csd/CARDDEMO.CSD`, `COMEN02Y` option 9 |
| batch statement programs | `CBSTM03A.CBL`, `CBSTM03B.CBL` (upper-case `.CBL` extension) | `app/cbl/` |
| — | CSD also defines transaction `CDV1` → program **`COCRDSEC`**, which **does not exist** in the repo (no source, not in any menu) | `app/csd/CARDDEMO.CSD` → no target; recorded as a gap |
| — | Map names differ from mapset names: e.g. mapset `COACTVW` / map `CACTVWA`, mapset `COCRDLI` / map `CCRDLIA` | `app/bms/*.bms` (`DFHMDI`) |
| — | Legacy field misspellings: `ACCT-EXPIRAION-DATE`, `CARD-EXPIRAION-DATE`, DB2 `MERCHANT_CATAGORY_CODE` | `app/cpy/CVACT01Y.cpy`, `app/cpy/CVACT02Y.cpy`, `app/app-authorization-ims-db2-mq/ddl/AUTHFRDS.ddl` → corrected in `data-model.md` |

## 1. Online CICS programs

Common to all BMS programs: COMMAREA `CARDDEMO-COMMAREA` (`app/cpy/COCOM01Y.cpy`, 160 bytes, extended
per-program after the common part), copybooks `COTTL01Y`, `CSDAT01Y`, `CSMSG01Y`, `DFHAID`, `DFHBMSCA`;
navigation via `XCTL` to `CDEMO-TO-PROGRAM` (menu/back = PF3). On AWS: React Router navigation + REST; no
server-side conversational state (JWT carries user id/role, screen context in route/query params).

### 1.1 Core (`app/cbl/`, CSD `app/csd/CARDDEMO.CSD`)

| Program | Tran | Mapset / map | Files / queues (CICS resource) | CICS operations | COMMAREA | Calls (XCTL/LINK/CALL) | LOC (lines) | Cx | Target | Owner |
|---|---|---|---|---|---|---|---|---|---|---|
| `COSGN00C` | `CC00` | `COSGN00` / `COSGN0A` | `USRSEC` (read) | `ASSIGN`, `READ`, `SEND/RECEIVE MAP`, `SEND TEXT`, `RETURN TRANSID`, `XCTL` | `COCOM01Y` | `COADM01C` (type A), `COMEN01C` (type U) | 172 (260) | L | `POST /api/v1/auth/signon`; route `/login` | ON, FE |
| `COMEN01C` | `CM00` | `COMEN01` / `COMEN1A` | — (menu table `COMEN02Y`) | `SEND/RECEIVE MAP`, `INQUIRE PROGRAM`, `XCTL`, `RETURN` | `COCOM01Y` | option programs from `COMEN02Y` | 213 (308) | L | `GET /api/v1/menus/main`; route `/menu` | ON, FE |
| `COADM01C` | `CA00` | `COADM01` / `COADM1A` | — (menu table `COADM02Y`) | `SEND/RECEIVE MAP`, `HANDLE CONDITION`, `XCTL`, `RETURN` | `COCOM01Y` | option programs from `COADM02Y` | 189 (288) | L | `GET /api/v1/menus/admin` (ADMIN); route `/admin` | ON, FE |
| `COACTVWC` | `CAVW` | `COACTVW` / `CACTVWA` | `CXACAIX` (xref by acct), `ACCTDAT`, `CUSTDAT` (read) | `READ`, `SEND/RECEIVE MAP`, `SEND TEXT`, `HANDLE ABEND`, `ABEND`, `XCTL` | `COCOM01Y` + local | `COMEN01C`, card programs via PF keys | 703 (941) | M | `GET /api/v1/accounts/{acctId}`; route `/accounts/view` | ON, FE |
| `COACTUPC` | `CAUP` | `COACTUP` / `CACTUPA` | `CXACAIX`, `ACCTDAT`, `CUSTDAT` (read; `READ UPDATE` + `REWRITE` ACCTDAT & CUSTDAT) | `READ`, `READ UPDATE`, `REWRITE`, `SYNCPOINT`, `SEND/RECEIVE MAP`, `HANDLE ABEND`, `XCTL` | `COCOM01Y` + local | `COMEN01C`; copybooks `CSUTLDPY`/`CSUTLDWY` (date edit), `CSLKPCDY` (state/ZIP/area-code tables), `CSSETATY`, `CSSTRPFY` | 3368 (4236) | H | `PUT /api/v1/accounts/{acctId}` (one DB txn account+customer, optimistic lock); route `/accounts/update` | ON, FE |
| `COCRDLIC` | `CCLI` | `COCRDLI` / `CCRDLIA` | `CARDDAT` (browse) | `STARTBR`, `READNEXT`, `READPREV`, `ENDBR`, `SEND/RECEIVE MAP`, `SEND TEXT`, `XCTL` | `COCOM01Y` + local | `COCRDSLC` (`S`), `COCRDUPC` (`U`), `COMEN01C` | 1093 (1459) | H | `GET /api/v1/cards` (keyset, 7 rows); route `/cards` | ON, FE |
| `COCRDSLC` | `CCDL` | `COCRDSL` / `CCRDSLA` | `CARDDAT` (by card), `CARDAIX` (by acct) | `READ`, `SEND/RECEIVE MAP`, `SEND TEXT`, `HANDLE ABEND`, `XCTL` | `COCOM01Y` + local | `COCRDLIC`, `COMEN01C` | 642 (887) | M | `GET /api/v1/cards/{cardNum}`; route `/cards/view` | ON, FE |
| `COCRDUPC` | `CCUP` | `COCRDUP` / `CCRDUPA` | `CARDDAT` (`READ UPDATE`, `REWRITE`) | `READ`, `REWRITE`, `SYNCPOINT`, `SEND/RECEIVE MAP`, `HANDLE ABEND`, `XCTL` | `COCOM01Y` + local | `COCRDLIC`, `COMEN01C` | 1195 (1560) | H | `PUT /api/v1/cards/{cardNum}`; route `/cards/update` | ON, FE |
| `COTRN00C` | `CT00` | `COTRN00` / `COTRN0A` | `TRANSACT` (browse) | `STARTBR`, `READNEXT`, `READPREV`, `ENDBR`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + `CDEMO-CT00-INFO` | `COTRN01C` (select), `COMEN01C` | 529 (699) | M | `GET /api/v1/transactions` (keyset, 10 rows); route `/transactions` | ON, FE |
| `COTRN01C` | `CT01` | `COTRN01` / `COTRN1A` | `TRANSACT` (read) | `READ`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + local | `COTRN00C`, `COMEN01C` | 231 (330) | L | `GET /api/v1/transactions/{tranId}`; route `/transactions/view` | ON, FE |
| `COTRN02C` | `CT02` | `COTRN02` / `COTRN2A` | `CXACAIX`, `CCXREF` (read), `TRANSACT` (`STARTBR`/`READPREV` for last id, `WRITE`) | `READ`, `STARTBR`, `READPREV`, `ENDBR`, `WRITE`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + local | `CALL 'CSUTLDTC'` (date validation), `COMEN01C` | 614 (783) | M | `POST /api/v1/transactions`; route `/transactions/new` | ON, FE |
| `CORPT00C` | `CR00` | `CORPT00` / `CORPT0A` | TD queue `JOBS` (write JCL deck) | `WRITEQ TD`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + local | `CALL 'CSUTLDTC'`; submits `TRANREPT` proc | 498 (649) | M | `POST /api/v1/reports/transactions` → SQS `carddemo-report-request` → Step Functions `carddemo-report`; route `/reports` | ON, FE, BA |
| `COBIL00C` | `CB00` | `COBIL00` / `COBIL0A` | `ACCTDAT` (`READ UPDATE`, `REWRITE`), `CXACAIX` (read), `TRANSACT` (`STARTBR`/`READPREV`, `WRITE`) | `READ`, `REWRITE`, `STARTBR`, `READPREV`, `ENDBR`, `WRITE`, `ASKTIME`, `FORMATTIME`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + local | `COMEN01C` | 420 (572) | M | `GET/POST /api/v1/bill-payments`; route `/bill-payment` | ON, FE |
| `COUSR00C` | `CU00` | `COUSR00` / `COUSR0A` | `USRSEC` (browse) | `STARTBR`, `READNEXT`, `READPREV`, `ENDBR`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + `CDEMO-CU00-INFO` | `COUSR02C` (`U`), `COUSR03C` (`D`), `COADM01C` | 531 (695) | M | `GET /api/v1/users` (ADMIN, 10 rows); route `/admin/users` | ON, FE |
| `COUSR01C` | `CU01` | `COUSR01` / `COUSR1A` | `USRSEC` (write) | `WRITE`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` | `COADM01C` | 198 (299) | L | `POST /api/v1/users`; route `/admin/users/new` | ON, FE |
| `COUSR02C` | `CU02` | `COUSR02` / `COUSR2A` | `USRSEC` (`READ UPDATE`, `REWRITE`) | `READ`, `REWRITE`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + local | `COADM01C`, `COUSR00C` | 303 (414) | L | `GET/PUT /api/v1/users/{userId}`; route `/admin/users/:userId/edit` | ON, FE |
| `COUSR03C` | `CU03` | `COUSR03` / `COUSR3A` | `USRSEC` (`READ UPDATE`, `DELETE`) | `READ`, `DELETE`, `SEND/RECEIVE MAP`, `XCTL` | `COCOM01Y` + local | `COADM01C`, `COUSR00C` | 251 (359) | L | `GET/DELETE /api/v1/users/{userId}`; route `/admin/users/:userId/delete` | ON, FE |

### 1.2 Optional sub-apps

| Program | Tran | Mapset / map | Resources | Operations | COMMAREA | Calls | LOC (lines) | Cx | Target | Owner |
|---|---|---|---|---|---|---|---|---|---|---|
| `COPAUA0C` (`app/app-authorization-ims-db2-mq/cbl/`) | `CP00` (MQ-triggered, CSD `CRDDEMO2.csd`) | none | MQ request queue (from trigger `RETRIEVE`), reply queue (`MQPUT1`); `CCXREF`, `ACCTDAT`, `CUSTDAT` (read); IMS `PAUTSUM0`/`PAUTDTL1` via PSB `PSBPAUTB` (`GU`, `ISRT`, `REPL`); TD queue for errors (`CCPAUERY`) | `RETRIEVE`, `READ`, `ASKTIME`, `FORMATTIME`, `WRITEQ TD`, `SYNCPOINT`, `RETURN`; `EXEC DLI SCHD/GU/ISRT/REPL/TERM`; `MQOPEN`/`MQGET`/`MQPUT1`/`MQCLOSE` | MQ trigger message (`MQTM`) | — | 771 (1026) | H | Authorization consumer on SQS `carddemo-pauth-request` → `carddemo-pauth-reply` (`messaging.md` §4) | ON (**replatform candidate**, §9) |
| `COPAUS0C` | `CPVS` | `COPAU00` / `COPAU0A` | `ACCTDAT`, `CXACAIX`, `CUSTDAT` (read); IMS `PAUTSUM0` `GU`, `PAUTDTL1` `GNP` | `READ`, `SEND/RECEIVE MAP`, `SYNCPOINT`, `XCTL`; DLI | `COCOM01Y` + `CDEMO-CPVS-*` | `COPAUS1C`, `COMEN01C` | 792 (1032) | H | `GET /api/v1/authorizations/{acctId}`; route `/authorizations` | ON, FE (**replatform candidate**) |
| `COPAUS1C` | `CPVD` | `COPAU01` / `COPAU1A` | IMS `PAUTSUM0`/`PAUTDTL1` (`GU`, `GNP`, `REPL`) | `SEND/RECEIVE MAP`, `LINK`, `SYNCPOINT`, `XCTL`; DLI | `COCOM01Y` + local | `LINK COPAUS2C` (fraud), `COPAUS0C` | 461 (604) | M | `GET /api/v1/authorizations/{acctId}/{authKey}`, `POST …/fraud`; route `/authorizations/:acctId/:authKey` | ON, FE (**replatform candidate**) |
| `COPAUS2C` | — (LINKed) | none | DB2 `CARDDEMO.AUTHFRDS` (`INSERT`, `UPDATE` on `-803`) | `ASKTIME`, `FORMATTIME`, `RETURN`; `EXEC SQL` | `CIPAUDTY` + fraud flag | — | 202 (244) | L | service method behind `POST /authorizations/…/fraud` (table `authfrds`) | ON |
| `COTRTLIC` (`app/app-transaction-type-db2/cbl/`) | `CTLI` (CSD `CRDDEMOD.csd`) | `COTRTLI` / `CTRTLIA` | DB2 `TRANSACTION_TYPE` (cursor browse, `UPDATE`, `DELETE`) | `SEND/RECEIVE MAP`, `SEND TEXT`, `SYNCPOINT`, `XCTL`; `EXEC SQL` cursors; `DSNTIAC` | `COCOM01Y` + local | `COTRTUPC`, `COADM01C` | 1861 (2098) | H | `GET/PUT/DELETE /api/v1/transaction-types`; route `/admin/transaction-types` | ON, FE |
| `COTRTUPC` | `CTTU` | `COTRTUP` / `CTRTUPA` | DB2 `TRANSACTION_TYPE` (`SELECT`, `INSERT`, `UPDATE`, `DELETE`), `TRANSACTION_TYPE_CATEGORY` (DCL included) | `SEND/RECEIVE MAP`, `SEND FROM`, `SYNCPOINT`, `HANDLE ABEND`, `XCTL`; `EXEC SQL` | `COCOM01Y` + local | `COTRTLIC`, `COADM01C` | 1429 (1702) | H | `GET/POST/PUT/DELETE /api/v1/transaction-types/{typeCd}`; route `/admin/transaction-types/maintain` | ON, FE |
| `COACCT01` (`app/app-vsam-mq/cbl/`) | `CDRA` (CSD `CRDDEMOM.csd`) | none | MQ `CARD.DEMO.REPLY.ACCT` (reply), `CARD.DEMO.ERROR`, input queue from trigger; `ACCTDAT` (read) | `RETRIEVE`, `READ`, `SYNCPOINT`, `RETURN`; `MQOPEN`/`MQGET`/`MQPUT`/`MQCLOSE` | MQ trigger message | — | 601 (620) | M | SQS consumer `carddemo-acct-inquiry-request` → `carddemo-acct-inquiry-reply` (`messaging.md` §3.1–3.2) | ON |
| `CODATE01` | `CDRD` | none | MQ `CARD.DEMO.REPLY.DATE`, `CARD.DEMO.ERROR`, input from trigger | `RETRIEVE`, `ASKTIME`, `FORMATTIME`, `SYNCPOINT`, `RETURN`; MQ API | MQ trigger message | — | 508 (524) | L | SQS consumer `carddemo-date-inquiry-request` → `carddemo-date-inquiry-reply` (`messaging.md` §3.3–3.4) | ON |

## 2. Batch programs, utilities and assembler

| Program | Source | Driving JCL | Input DDs → datasets | Output DDs → datasets | LOC (lines) | Target (`batch.md` job / state) | Owner |
|---|---|---|---|---|---|---|---|
| `CBACT01C` | `app/cbl/CBACT01C.cbl` | `READACCT.jcl` | `ACCTFILE` → `ACCTDATA.VSAM.KSDS` | `OUTFILE` → `ACCTDATA.PSCOMP`, `ARRYFILE` → `ACCTDATA.ARRYPS`, `VBRCFILE` → `ACCTDATA.VBPS` (V 10–80) | 358 (430) | `extract-accounts` → `s3://<bucket>/extract/account/<runId>/`; `CALL 'COBDATFT'` → `java.time` | BA |
| `CBACT02C` | `app/cbl/CBACT02C.cbl` | `READCARD.jcl` | `CARDFILE` → `CARDDATA.VSAM.KSDS` | SYSOUT (`DISPLAY`) | 129 (178) | `print-cards` (CloudWatch Logs) | BA |
| `CBACT03C` | `app/cbl/CBACT03C.cbl` | `READXREF.jcl` | `XREFFILE` → `CARDXREF.VSAM.KSDS` | SYSOUT | 130 (178) | `print-xref` | BA |
| `CBCUS01C` | `app/cbl/CBCUS01C.cbl` | `READCUST.jcl` | `CUSTFILE` → `CUSTDATA.VSAM.KSDS` | SYSOUT | 130 (178) | `print-customers` | BA |
| `CBACT04C` | `app/cbl/CBACT04C.cbl` | `INTCALC.jcl` (`PARM='2022071800'`) | `TCATBALF` → `TCATBALF.VSAM.KSDS`, `XREFFILE` → `CARDXREF.VSAM.KSDS`, `XREFFIL1` → `CARDXREF.VSAM.AIX.PATH`, `ACCTFILE` → `ACCTDATA.VSAM.KSDS`, `DISCGRP` → `DISCGRP.VSAM.KSDS` | `TRANSACT` → `SYSTRAN(+1)`; `REWRITE` ACCTFILE | 552 (652) | `calculate-interest` (state in `carddemo-monthly-interest`); fees paragraph incomplete in source (§9) | BA |
| `CBTRN01C` | `app/cbl/CBTRN01C.cbl` | **none** (no JCL references it) | `DALYTRAN`, `CUSTFILE`, `XREFFILE`, `CARDFILE`, `ACCTFILE`, `TRANFILE` | SYSOUT | 415 (494) | `validate-daily-transactions` (optional) | BA |
| `CBTRN02C` | `app/cbl/CBTRN02C.cbl` | `POSTTRAN.jcl` | `DALYTRAN` → `DALYTRAN.PS`, `XREFFILE` → `CARDXREF.VSAM.KSDS` | `TRANFILE` → `TRANSACT.VSAM.KSDS` (write), `ACCTFILE` (rewrite), `TCATBALF` (write/rewrite), `DALYREJS` → `DALYREJS(+1)` (350+80) | 619 (731) | `post-daily-transactions` (state in `carddemo-daily-cycle`); RC 4 on rejects | BA |
| `CBTRN03C` | `app/cbl/CBTRN03C.cbl` | `TRANREPT.jcl` / `app/proc/TRANREPT.prc` STEP10R | `TRANFILE` → `TRANSACT.DALY(+1)` (sorted by card), `CARDXREF`, `TRANTYPE` → `TRANTYPE.VSAM.KSDS`, `TRANCATG` → `TRANCATG.VSAM.KSDS`, `DATEPARM` → `DATEPARM` | `TRANREPT` → `TRANREPT(+1)` (133) | 545 (649) | `transaction-report` → `s3://<bucket>/reports/tranrept/…` | BA |
| `CBSTM03A` | `app/cbl/CBSTM03A.CBL` | `CREASTMT.JCL` STEP040 | `TRNXFILE` → `TRXFL.VSAM.KSDS`, `XREFFILE`, `ACCTFILE`, `CUSTFILE` (via `CBSTM03B`) | `STMTFILE` → `STATEMNT.PS` (80), `HTMLFILE` → `STATEMNT.HTML` (100) | 784 (924) | `create-statements` → `s3://<bucket>/statements/…` | BA |
| `CBSTM03B` | `app/cbl/CBSTM03B.CBL` | called by `CBSTM03A` | file I/O subroutine (ops `O`,`C`,`R`,`K` on DD name in `WS-M03B-DD`) | — | 162 (230) | repository interface inside `create-statements` | BA |
| `CBEXPORT` | `app/cbl/CBEXPORT.cbl` | `CBEXPORT.jcl` STEP02 | `CUSTFILE`, `ACCTFILE`, `XREFFILE`, `TRANSACT`, `CARDFILE` (KSDS) | `EXPFILE` → `EXPORT.DATA` (VSAM, 500, `CVEXPORT`) | 396 (582) | `export-customer-data` → `s3://<bucket>/export/…` | BA |
| `CBIMPORT` | `app/cbl/CBIMPORT.cbl` | `CBIMPORT.jcl` | `EXPFILE` → `EXPORT.DATA` | `CUSTOUT`, `ACCTOUT`, `XREFOUT`, `TRNXOUT` (`*.IMPORT`), `ERROUT` → `IMPORT.ERRORS` (132) | 337 (487) | `import-customer-data` → `s3://<bucket>/import/…` | BA |
| `CSUTLDTC` | `app/cbl/CSUTLDTC.cbl` | called by `COTRN02C`, `CORPT00C` | `USING LS-DATE, LS-DATE-FORMAT, LS-RESULT`; `CALL 'CEEDAYS'` (LE) | — | 114 (157) | shared Java date validator (`com.carddemo.common`) | ON, BA |
| `COBSWAIT` | `app/cbl/COBSWAIT.cbl` | `WAITSTEP.jcl` | SYSIN: 8-digit centiseconds | — | 13 (41) | Step Functions `Wait` state (not a job) | BA, IN |
| `COBDATFT` | `app/asm/COBDATFT.asm` (+ `app/maclib/COCDATFT.mac`) | called by `CBACT01C` | date reformat (input type `1`/`2`) | — | 82 (84) | `java.time` formatter; **not ported as code** (§9) | BA |
| `MVSWAIT` | `app/asm/MVSWAIT.asm` (+ `app/maclib/ASMWAIT.mac`) | called by `COBSWAIT` | `STIMER` wait | — | 28 (30) | Step Functions `Wait`; not needed on AWS | BA |
| `CBPAUP0C` | `app/app-authorization-ims-db2-mq/cbl/CBPAUP0C.cbl` | `CBPAUP0J.jcl` (`DFSRRC00` BMP, PSB `PSBPAUTB`) | IMS `PAUTSUM0`/`PAUTDTL1`; SYSIN `P-EXPIRY-DAYS`, `P-CHKP-FREQ`, `P-CHKP-DIS-FREQ` | IMS deletes; RC 16 on error | 266 (386) | `purge-expired-authorizations` (**replatform candidate**) | BA |
| `PAUDBUNL` | `…/cbl/PAUDBUNL.CBL` | `UNLDPADB.JCL` | IMS `DBPAUTP0` | `OUTFIL1` → `PAUTDB.ROOT.FILEO`, `OUTFIL2` → `PAUTDB.CHILD.FILEO` | 222 (317) | one-time extract for DM; not needed after cut-over | DM |
| `PAUDBLOD` | `…/cbl/PAUDBLOD.CBL` | `LOADPADB.JCL` | `INFILE1`/`INFILE2` (`PAUTDB.*.FILEO`) | IMS `DBPAUTP0` | 274 (369) | not needed on AWS (DM loader loads Aurora) | DM |
| `DBUNLDGS` | `…/cbl/DBUNLDGS.CBL` | `UNLDGSAM.JCL` | IMS `DBPAUTP0` | GSAM `PASFILOP` → `PAUTDB.ROOT.GSAM`, `PADFILOP` → `PAUTDB.CHILD.GSAM` | 211 (366) | not needed on AWS | DM |
| `COBTUPDT` | `app/app-transaction-type-db2/cbl/COBTUPDT.cbl` | `MNTTRDB2.jcl` (`IKJEFT01` DSN RUN) | `INPFILE` (col1 `A`/`U`/`D`/`*`, 2–3 type, 4–53 desc) | DB2 `TRANSACTION_TYPE`; RC 4 on SQL error | 205 (237) | `maintain-transaction-types` | BA |

## 3. JCL, procedures, control cards, scheduler

Utilities: `IDCAMS` (VSAM define/delete/REPRO/AIX), `SORT` (DFSORT), `IEBGENER` (copy), `IEFBR14`
(allocate/delete), `SDSF` (issue CICS `CEMT SET FILE CLOSE/OPEN`), `DFHCSDUP` (CSD update), `FTP`,
`IKJEFT01/1B` (TSO batch: DB2 DSN RUN, REXX `TXT2PDF`), `DFSRRC00` (IMS batch region), `DFSURGU0` (IMS unload).
Targets use job names from [`contracts/batch.md`](contracts/batch.md) §2; "not needed" = no AWS artifact, reason given.

### 3.1 Core `app/jcl/` (38 files)

| JCL | Purpose | Steps : program | Datasets (`AWS.M2.CARDDEMO.` omitted) | AWS target | Owner |
|---|---|---|---|---|---|
| `ACCTFILE.jcl` | (Re)define account KSDS and load from PS | STEP05/10/15: IDCAMS | `ACCTDATA.PS` → `ACCTDATA.VSAM.KSDS` | DM loader → `account`; `load-reference-data --table=account`; DEFINE not needed (Flyway DDL) | DM, BA |
| `CARDFILE.jcl` | Close CICS file, (re)define card KSDS + AIX `CARDAIX`, load, reopen | CLCIFIL: SDSF; STEP05–60: IDCAMS; OPCIFIL: SDSF | `CARDDATA.PS` → `CARDDATA.VSAM.KSDS` (+AIX/PATH) | DM loader → `card` (+ index `ix_card_acct_id`) | DM |
| `CBADMCDJ.jcl` | Define CardDemo CSD group (programs, maps, transactions, files) | STEP1: DFHCSDUP | `OEM.CICSTS.DFHCSD` | not needed — routes/endpoints replace CSD (`api.md`), infra in IaC | IN |
| `CBEXPORT.jcl` | Export all entities to multi-record file | STEP01: IDCAMS (define `EXPORT.DATA`); STEP02: `CBEXPORT` | 5 KSDS → `EXPORT.DATA` | `export-customer-data` | BA |
| `CBIMPORT.jcl` | Import/split export file | STEP01: `CBIMPORT` | `EXPORT.DATA` → `*.IMPORT`, `IMPORT.ERRORS` | `import-customer-data` | BA |
| `CLOSEFIL.jcl` | Close CICS files before batch | CLCIFIL: SDSF | — | not needed — Aurora allows concurrent online/batch (row locks) | BA |
| `COMBTRAN.jcl` | Merge backup + interest transactions, reload KSDS | STEP05R: SORT; STEP10: IDCAMS REPRO | `TRANSACT.BKUP(0)` + `SYSTRAN(0)` → `TRANSACT.COMBINED(+1)` → `TRANSACT.VSAM.KSDS` | `combine-transactions` (verification no-op: interest rows written directly to `transaction`) | BA |
| `CREASTMT.JCL` | Build statements (text + HTML) | DELDEF01: IDCAMS; STEP010: SORT (card+tran id); STEP020: IDCAMS REPRO → `TRXFL.VSAM.KSDS`; STEP030: IEFBR14; STEP040: `CBSTM03A` | `TRANSACT.VSAM.KSDS` → `TRXFL.SEQ`/`TRXFL.VSAM.KSDS` → `STATEMNT.PS`, `STATEMNT.HTML` | `create-statements` (SQL `ORDER BY card_num, tran_id` replaces SORT/TRXFL) | BA |
| `CUSTFILE.jcl` | Close, (re)define/load customer KSDS, open | CLCIFIL; STEP05–15: IDCAMS; OPCIFIL | `CUSTDATA.PS` → `CUSTDATA.VSAM.KSDS` | DM loader → `customer` | DM |
| `DALYREJS.jcl` | Define GDG base `DALYREJS` (LIMIT 5) | STEP05: IDCAMS | `DALYREJS` GDG | S3 prefix `output/dalyrejs/`; not needed as a job | IN |
| `DEFCUST.jcl` | Legacy define of a customer cluster (names `AWS.CCDA.CUSTDATA.CLUSTER`/`AWS.CUSTDATA.CLUSTER`, not used by other JCL) | STEP05: IDCAMS ×2 | — | not needed (superseded by `CUSTFILE.jcl`) | — |
| `DEFGDGB.jcl` | Define GDG bases `TRANSACT.BKUP`, `TRANSACT.DALY`, `TRANREPT`, `TCATBALF.BKUP`, `SYSTRAN`, `TRANSACT.COMBINED` (LIMIT 5) | STEP05: IDCAMS | GDG bases | S3 prefixes (`batch.md` §1.2); not needed as a job | IN |
| `DEFGDGD.jcl` | Define GDGs for `TRANTYPE.BKUP`, `TRANCATG.PS.BKUP`, `DISCGRP.BKUP` and copy first generation | STEP10/30/50: IDCAMS; STEP20/40/60: IEBGENER | `*.PS` → `*.BKUP(+1)` | `backup-reference-data` | BA |
| `DISCGRP.jcl` | (Re)define/load disclosure group KSDS | STEP05–15: IDCAMS | `DISCGRP.PS` → `DISCGRP.VSAM.KSDS` | `load-reference-data --table=disclosure_group` | BA, DM |
| `DUSRSECJ.jcl` | Create user security PS from in-stream records, define/load KSDS | PREDEL: IEFBR14; STEP01: IEBGENER; STEP02/03: IDCAMS | in-stream → `USRSEC.PS` → `USRSEC.VSAM.KSDS` | DM seed → `user_security` (passwords hashed, `data-model.md` §2.1) | DM |
| `ESDSRRDS.jcl` | Demo: user security as ESDS and RRDS | PREDEL; STEP01: IEBGENER; STEP02–05: IDCAMS | `ESDSRRDS.PS` → `USRSEC.VSAM.ESDS`/`.RRDS` | not needed (demo of VSAM organizations; no program reads them) | — |
| `FTPJCL.JCL` | FTP a file to/from remote host | STEP1: FTP | — | not needed — S3 is the exchange point | — |
| `INTCALC.jcl` | Monthly interest | STEP15: `CBACT04C` `PARM='2022071800'` | see §2 | `calculate-interest` | BA |
| `INTRDRJ1.JCL` | Demo: copy file and submit `INTRDRJ2` via internal reader | IDCAMS; STEP01: IEBGENER → `INTRDR` | `AWS.M2.CARDEMO.FTP.TEST*` | not needed — Step Functions chaining | — |
| `INTRDRJ2.JCL` | Demo: second job of internal-reader pair | IDCAMS | `…FTP.TEST.BKUP*` | not needed | — |
| `OPENFIL.jcl` | Reopen CICS files after batch | OPCIFIL: SDSF | — | not needed (see `CLOSEFIL`) | — |
| `POSTTRAN.jcl` | Daily transaction posting | STEP15: `CBTRN02C` | see §2 | `post-daily-transactions` | BA |
| `PRTCATBL.jcl` | Unload + sort category balances for print | DELDEF: IEFBR14; STEP05R: `REPROC`; STEP10R: SORT | `TCATBALF.VSAM.KSDS` → `TCATBALF.BKUP(+1)` → `TCATBALF.REPT` | `category-balance-report` | BA |
| `READACCT.jcl` | Account extract (3 formats) | PREDEL: IEFBR14; STEP05: `CBACT01C` | see §2 | `extract-accounts` | BA |
| `READCARD.jcl` | Print cards | STEP05: `CBACT02C` | `CARDDATA.VSAM.KSDS` | `print-cards` | BA |
| `READCUST.jcl` | Print customers | STEP05: `CBCUS01C` | `CUSTDATA.VSAM.KSDS` | `print-customers` | BA |
| `READXREF.jcl` | Print xref | STEP05: `CBACT03C` | `CARDXREF.VSAM.KSDS` | `print-xref` | BA |
| `REPTFILE.jcl` | Define GDG base `TRANREPT` (LIMIT 10) | STEP05: IDCAMS | GDG | S3 prefix `reports/tranrept/`; not needed as a job | IN |
| `TCATBALF.jcl` | (Re)define/load category balance KSDS | STEP05–15: IDCAMS | `TCATBALF.PS` → `TCATBALF.VSAM.KSDS` | DM loader / `load-reference-data --table=tran_cat_balance` | DM, BA |
| `TRANBKP.jcl` | Back up transactions, redefine KSDS + AIX | STEP05R: `REPROC`; STEP05/10: IDCAMS | `TRANSACT.VSAM.KSDS` → `TRANSACT.BKUP(+1)` | `backup-transactions`; redefine not needed | BA |
| `TRANCATG.jcl` | (Re)define/load transaction category KSDS | STEP05–15: IDCAMS | `TRANCATG.PS` → `TRANCATG.VSAM.KSDS` | `load-reference-data --table=transaction_category` | DM, BA |
| `TRANFILE.jcl` | Close, (re)define transaction KSDS + AIX, load initial record, open | CLCIFIL; STEP05–30: IDCAMS; OPCIFIL | `DALYTRAN.PS.INIT` → `TRANSACT.VSAM.KSDS` | not needed: Aurora tables may be empty; `transaction` + `ix_transaction_proc_ts` created by `aws/db/schema.sql` | DM |
| `TRANIDX.jcl` | Define transaction AIX/PATH, BLDINDEX | STEP20/25/30: IDCAMS | `TRANSACT.VSAM.AIX` | Flyway index `ix_transaction_proc_ts`; not needed as a job | DM |
| `TRANREPT.jcl` | Transaction detail report for date range | STEP05R: `REPROC` unload; STEP05R: SORT (filter/sort); STEP10R: `CBTRN03C` | see §2 | `transaction-report` | BA |
| `TRANTYPE.jcl` | (Re)define/load transaction type KSDS | STEP05–15: IDCAMS | `TRANTYPE.PS` → `TRANTYPE.VSAM.KSDS` | `load-reference-data --table=transaction_type` | DM, BA |
| `TXT2PDF1.JCL` | Convert statement text to PDF (REXX `TXT2PDF`) | TXT2PDF: IKJEFT1B | `STATEMNT.PS`; `AWS.M2.LBD.TXT2PDF.*` | `statement-pdf` (Java PDF library; REXX not ported, §9) | BA |
| `WAITSTEP.jcl` | Pause (`COBSWAIT`, SYSIN `00003600` = 36 s) | WAIT: `COBSWAIT` | — | Step Functions `Wait` state | BA |
| `XREFFILE.jcl` | (Re)define/load xref KSDS + AIX `CXACAIX` | STEP05–30: IDCAMS | `CARDXREF.PS` → `CARDXREF.VSAM.KSDS` (+AIX) | DM loader → `card_xref` (+ index `ix_card_xref_acct_id`) | DM |

### 3.2 Optional sub-app JCL (8 files)

| JCL | Purpose | Steps : program | AWS target | Owner |
|---|---|---|---|---|
| `app/app-authorization-ims-db2-mq/jcl/CBPAUP0J.jcl` | Purge expired pending authorizations (BMP) | STEP01: DFSRRC00 → `CBPAUP0C` | `purge-expired-authorizations` (only if IMS refactored; else replatform) | BA |
| `…/jcl/DBPAUTP0.jcl` | IMS HD unload of `DBPAUTP0` | STEPDEL: IEFBR14; UNLOAD: DFSRRC00 (`DFSURGU0`) | one-time DM extract; not needed after cut-over | DM |
| `…/jcl/LOADPADB.JCL` | Load IMS DB from sequential | STEP01: DFSRRC00 → `PAUDBLOD` | not needed | — |
| `…/jcl/UNLDGSAM.JCL` | Unload IMS DB to GSAM | STEP01: DFSRRC00 → `DBUNLDGS` | not needed | — |
| `…/jcl/UNLDPADB.JCL` | Unload IMS DB to sequential root/child files | STEP0: IEFBR14; STEP01: DFSRRC00 → `PAUDBUNL` | `unload-auth-db` one-time DM extract | DM |
| `app/app-transaction-type-db2/jcl/CREADB21.jcl` | Create DB2 DB/tables, load, free plans | FREEPLN, CRCRDDB, RUNTEP2, LDTCCAT: IKJEFT01; LDTTYPE: IEFBR14 (ctl `DB2CREAT`, `DB2FREE`, `DB2TEP41`, `DB2TIAD1`, `DB2LTTYP`, `DB2LTCAT`) | Flyway migrations + seed (DM); not needed as a job | DM |
| `…/jcl/MNTTRDB2.jcl` | Batch maintenance of `TRANSACTION_TYPE` | STEP1: IKJEFT01 DSN RUN `COBTUPDT` | `maintain-transaction-types` | BA |
| `…/jcl/TRANEXTR.jcl` | Back up VSAM PS files, extract DB2 type/category to PS (feeds VSAM refresh) | STEP10/20: IEBGENER; STEP30: IEFBR14; STEP40/50: IKJEFT01 (`DSNTIAUL`, ctl `DB2LTTYP`/`DB2LTCAT`) | `extract-transaction-types` (+ `backup-reference-data`) | BA |

### 3.3 Procedures, control cards, samples

| Artifact | Purpose | AWS target |
|---|---|---|
| `app/proc/REPROC.prc` | IDCAMS `REPRO` unload proc (ctl `app/ctl/REPROCT.ctl`) | not needed — SQL `SELECT` / table dump |
| `app/proc/TRANREPT.prc` | Unload→SORT→`CBTRN03C` proc used by `CORPT00C`-generated JCL | `transaction-report` |
| `app/ctl/REPROCT.ctl` (+ copy `app/app-transaction-type-db2/ctl/REPROCT.ctl`) | `REPRO` control statement | not needed |
| `app/app-transaction-type-db2/ctl/DB2CREAT.ctl`, `DB2FREE.ctl`, `DB2TEP41.ctl`, `DB2TIAD1.ctl`, `DB2LTTYP.ctl`, `DB2LTCAT.ctl` | DB2 create/free/bind/unload SQL | Flyway DDL (DM); unload SQL → `extract-transaction-types` |
| `samples/jcl/BATCMP.jcl`, `BMSCMP.jcl`, `CICCMP.jcl`, `CICDBCMP.jcl`, `IMSMQCMP.jcl` | Compile/link templates (batch, BMS, CICS, CICS+DB2, IMS+MQ) | not needed — Maven/Vite builds (`conventions.md`) |
| `samples/proc/BUILDBAT.prc`, `BUILDBMS.prc`, `BUILDONL.prc`, `BLDCIDB2.prc` | Compile procs (IGYCRCTL, DFHECP1, DSNHPC, ASMA90, HEWL) | not needed — CI build (IN) |
| `samples/jcl/LISTCAT.jcl`, `app/catlg/LISTCAT.txt` | IDCAMS LISTCAT of all CardDemo datasets (source of VSAM attributes in §6) | not needed; used as DM reference |
| `samples/jcl/RACFCMDS.jcl` | RACF transaction profiles | not needed — JWT roles (`api.md` §2) |
| `samples/jcl/REPRTEST.jcl`, `SORTTEST.jcl` | REPROC / SORT tests | not needed |
| `samples/m2/mf/CardDemo_runtime.zip`, `samples/m2/unikix/UniKix_CardDemo_runtime_v1.zip` | Prebuilt AWS M2 (Micro Focus / UniKix) runtime packages | Reference for replatform candidates (§9); not used by refactor |
| `app/maclib/ASMWAIT.mac`, `app/maclib/COCDATFT.mac` | Assembler macros for `MVSWAIT` / `COBDATFT` | not needed |
| `app/csd/CARDDEMO.CSD`, `app/app-authorization-ims-db2-mq/csd/CRDDEMO2.csd`, `app/app-transaction-type-db2/csd/CRDDEMOD.csd`, `app/app-vsam-mq/csd/CRDDEMOM.csd` | CICS resource definitions (61 / 9 / 6 / 4 DEFINEs) | source of §1 transaction IDs; replaced by `api.md` + IaC |

### 3.4 Scheduler (`app/scheduler/CardDemo.ca7`, `app/scheduler/CardDemo.controlm`)

CA-7 trigger edges (from `TRIGGERED JOBS`):

| SCHID | Chain |
|---|---|
| 030 | `CLOSEFIL → CBPAUP0J → POSTTRAN → WAITSTEP → OPENFIL` |
| 030 | `CLOSEFIL → CREASTMT → TXT2PDF1 → WAITSTEP → OPENFIL` |
| 030 | `CLOSEFIL → READACCT → READCARD → READCUST → READXREF → WAITSTEP` |
| 030→031→032 | `CLOSEFIL → TRANTYPE → WAITSTEP → CLOSEFIL1 → TRANCATG → WAITSTEP → CLOSEFIL2 → TCATBALF → WAITSTEP` |
| 031 | `OPENFIL → CLOSEFIL → PRTCATBL → WAITSTEP → OPENFIL` |

Control-M folders: `DAILY-TransactionBackup` (`DAYS="ALL"`): `CLOSEFIL → TRANBKP → WAITSTEP → OPENFIL`;
`WEEKLY-TransactionTypesDBRefresh` (`DAYS="SA"`): `MNTTRDB2 → TRANEXTR`; `WEEKLY-DisclosureGroupsRefresh`
(`DAYS="SA"`, after `MNTTRDB2`): `CLOSEFIL → DISCGRP → WAITSTEP → OPENFIL`; `MONTHLY-InterestCalculation`:
`CLOSEFIL → INTCALC → COMBTRAN → WAITSTEP → OPENFIL`.

**Daily cycle order on AWS** (authoritative in `batch.md` §3): `purge-expired-authorizations` (optional) →
`post-daily-transactions` → `backup-transactions`; month start: `calculate-interest` → `combine-transactions`
→ `create-statements` → `statement-pdf`; Saturdays: `maintain-transaction-types` → `extract-transaction-types`
→ disclosure-group refresh. `CLOSEFIL`/`OPENFIL` dropped, `WAITSTEP` → `Wait`. `TRANREPT` is not in either
scheduler (submitted on demand by `CR00`).

## 4. Copybooks

Length = computed from PIC clauses of the 01 level (display = digits, `COMP-3` = ⌊n/2⌋+1, `COMP` = 2/4/8).
"Used by" = programs with `COPY <name>` (or `EXEC SQL INCLUDE`). Flags: C3 = `COMP-3`, C = `COMP`/`BINARY`,
R = `REDEFINES`, O = `OCCURS`.

### 4.1 Core `app/cpy/` (30 files)

| Copybook | 01 level(s) | Len | Key field(s) | Flags | VSAM / file described | Used by | Target |
|---|---|---|---|---|---|---|---|
| `CSUSR01Y.cpy` | `SEC-USER-DATA` | 80 | `SEC-USR-ID X(08)` | — | `USRSEC.VSAM.KSDS` (CICS `USRSEC`) | `COSGN00C`, `COUSR00–03C`, `COMEN01C`, `COADM01C`, card/account programs, `COTRTLIC/UPC` | table `user_security` |
| `CVACT01Y.cpy` | `ACCOUNT-RECORD` | 300 | `ACCT-ID 9(11)` | — | `ACCTDATA.VSAM.KSDS` (`ACCTDAT`) | 16 programs (`CBACT01C`, `CBACT04C`, `CBTRN01C/02C`, `CBSTM03A`, `CBEXPORT/IMPORT`, `COACTVWC/UPC`, `COBIL00C`, `COCRDSLC/UPC`, `COTRN02C`, `COPAUA0C`, `COPAUS0C`, `COACCT01`) | `account` |
| `CVACT02Y.cpy` | `CARD-RECORD` | 150 | `CARD-NUM X(16)`; AIX `CARD-ACCT-ID 9(11)` @16 | — | `CARDDATA.VSAM.KSDS` (`CARDDAT`, AIX `CARDAIX`) | `CBACT02C`, `CBTRN01C`, `CBEXPORT/IMPORT`, `COACTVWC`, `COCRDLIC/SLC/UPC`, `COPAUS0C`, `COTRTLIC` | `card` |
| `CVACT03Y.cpy` | `CARD-XREF-RECORD` | 50 | `XREF-CARD-NUM X(16)`; AIX `XREF-ACCT-ID 9(11)` @25 | — | `CARDXREF.VSAM.KSDS` (`CCXREF`, AIX `CXACAIX`) | 16 programs incl. `CBACT03C`, `CBACT04C`, `CBTRN01–03C`, `COBIL00C`, `COTRN02C`, `COPAUA0C` | `card_xref` |
| `CVCUS01Y.cpy` | `CUSTOMER-RECORD` | 500 | `CUST-ID 9(09)` | — | `CUSTDATA.VSAM.KSDS` (`CUSTDAT`) | `CBCUS01C`, `CBTRN01C`, `CBEXPORT/IMPORT`, `COACTVWC/UPC`, `COCRDSLC/UPC`, `COPAUA0C`, `COPAUS0C` | `customer` |
| `CUSTREC.cpy` | `CUSTOMER-RECORD` | 500 | `CUST-ID` | — | same PICs as `CVCUS01Y`; field names differ (e.g. `CUST-DOB-YYYYMMDD` vs `CUST-DOB-YYYY-MM-DD`) | `CBSTM03A` | `customer` (no separate table) |
| `CVTRA01Y.cpy` | `TRAN-CAT-BAL-RECORD` | 50 | `TRANCAT-ACCT-ID 9(11)` + `TRANCAT-TYPE-CD X(02)` + `TRANCAT-CD 9(04)` (17) | — | `TCATBALF.VSAM.KSDS` | `CBACT04C`, `CBTRN02C` | `tran_cat_balance` |
| `CVTRA02Y.cpy` | `DIS-GROUP-RECORD` | 50 | `DIS-ACCT-GROUP-ID X(10)` + `DIS-TRAN-TYPE-CD X(02)` + `DIS-TRAN-CAT-CD 9(04)` (16) | — | `DISCGRP.VSAM.KSDS` | `CBACT04C` | `disclosure_group` |
| `CVTRA03Y.cpy` | `TRAN-TYPE-RECORD` | 60 | `TRAN-TYPE X(02)` | — | `TRANTYPE.VSAM.KSDS` | `CBTRN03C` | `transaction_type` |
| `CVTRA04Y.cpy` | `TRAN-CAT-RECORD` | 60 | `TRAN-TYPE-CD X(02)` + `TRAN-CAT-CD 9(04)` (6) | — | `TRANCATG.VSAM.KSDS` | `CBTRN03C` | `transaction_category` |
| `CVTRA05Y.cpy` | `TRAN-RECORD` | 350 | `TRAN-ID X(16)`; AIX `TRAN-PROC-TS X(26)` @304 | — | `TRANSACT.VSAM.KSDS` (`TRANSACT`), `SYSTRAN`, `TRANSACT.BKUP` | 11 programs (`COTRN00–02C`, `COBIL00C`, `CORPT00C`, `CBACT04C`, `CBTRN01–03C`, `CBEXPORT/IMPORT`) | `transaction` |
| `CVTRA06Y.cpy` | `DALYTRAN-RECORD` | 350 | `DALYTRAN-ID X(16)` | — | `DALYTRAN.PS` (sequential) | `CBTRN01C`, `CBTRN02C` | S3 `input/dalytran/` + `daily_transaction` |
| `CVTRA07Y.cpy` | `REPORT-NAME-HEADER`, `TRANSACTION-DETAIL-REPORT`, `TRANSACTION-HEADER-1/2`, + 3 total lines | 110–133 | — | — | `TRANREPT` GDG (FB 133) | `CBTRN03C` | S3 `reports/tranrept/` |
| `COSTM01.CPY` | `TRNX-RECORD` | 350 | `TRNX-CARD-NUM X(16)` + `TRNX-ID X(16)` (32) | — | `TRXFL.VSAM.KSDS` (statement work file) | `CBSTM03A` | none — SQL ordering (`batch.md` `create-statements`) |
| `CVEXPORT.cpy` | `EXPORT-RECORD` | 500 | `EXPORT-SEQUENCE-NUM 9(9) COMP` at offset 27; note `CBEXPORT.jcl` defines `KEYS(4 28)` (one byte off vs. copybook) — AWS keys on the sequence number | C3, C, R, O | `EXPORT.DATA` VSAM | `CBEXPORT`, `CBIMPORT` | S3 `export/` (fixed-width, `batch.md` §1.3) |
| `CODATECN.cpy` | `CODATECN-REC` | 80 | — | R | date-conversion parameter area for `COBDATFT` | `CBACT01C` | `java.time` (no table) |
| `COCOM01Y.cpy` | `CARDDEMO-COMMAREA` | 160 | — | — | CICS COMMAREA | all 21 BMS programs | JWT claims + route params (`api.md` §2) |
| `COMEN02Y.cpy` | `CARDDEMO-MAIN-MENU-OPTIONS` | 508 | — | R, O | main menu table (11 options, name/program/user type) | `COMEN01C` | `GET /menus/main` data |
| `COADM02Y.cpy` | `CARDDEMO-ADMIN-MENU-OPTIONS` | 272 | — | R, O | admin menu table | `COADM01C` | `GET /menus/admin` data |
| `COTTL01Y.cpy` | `CCDA-SCREEN-TITLE` | 120 | — | — | screen titles | 21 BMS programs | React layout header |
| `CSDAT01Y.cpy` | `WS-DATE-TIME` | 58 | — | R | current date/time work area | 21 BMS programs | `java.time` |
| `CSMSG01Y.cpy` | `CCDA-COMMON-MESSAGES` | 100 | — | — | common messages ("Thank you…", "Invalid key…") | 21 BMS programs | `api.md` §1 messages |
| `CSMSG02Y.cpy` | `ABEND-DATA` | 134 | — | — | abend code/culprit/message | 8 programs | error envelope `code`/`message` |
| `CSLKPCDY.cpy` | `WS-US-PHONE-AREA-CODE-TO-EDIT`, `US-STATE-CODE-TO-EDIT`, `US-STATE-ZIPCODE-TO-EDIT` (88-level tables) | 3/2/7 | — | — | validation lookup tables (area codes, states, state+ZIP prefix) | `COACTUPC` | Java validation constants (ON) |
| `CSUTLDWY.cpy` | working storage for date edit (`WS-EDIT-DATE-CCYYMMDD` …) | var | — | C, R | — | `COACTUPC`, `COTRTUPC` | shared Java date validator |
| `CSUTLDPY.cpy` | PROCEDURE DIVISION paragraphs (date edit; calls `CSUTLDTC`) | n/a | — | C | — | `COACTUPC` | shared Java date validator |
| `CSSETATY.cpy` | PROCEDURE DIVISION snippet (set field attributes on error) | n/a | — | — | — | `COACTUPC`, `COTRTUPC` | React field-error styling |
| `CSSTRPFY.cpy` | PROCEDURE DIVISION snippet (map `EIBAID` → PF key flags) | n/a | — | — | — | 7 programs | React keyboard shortcuts (optional) |
| `CVCRD01Y.cpy` | `CC-WORK-AREAS` | 213 | — | R | card/account work fields, PF-key flags | 7 programs | DTO fields |
| `UNUSED1Y.cpy` | `UNUSED-DATA` | 80 | — | — | not referenced by any program | — | not needed |

### 4.2 Optional sub-app copybooks (11 files) + DB2 DCLGEN (3) + IMS (8)

| Copybook | Layout | Len | Key | Flags | Describes | Used by | Target |
|---|---|---|---|---|---|---|---|
| `app/app-authorization-ims-db2-mq/cpy/CIPAUSMY.cpy` | `PA-ACCT-ID S9(11) C3`, `PA-CUST-ID`, status, limits/balances `S9(09)V99 C3`, counts, `PA-ACCOUNT-STATUS OCCURS 5` | 100 (segment) | `PA-ACCT-ID` | C3, C, O | IMS segment `PAUTSUM0` (`DBPAUTP0`) | `COPAUA0C`, `COPAUS0C/1C`, `CBPAUP0C`, `PAUDB*`, `DBUNLDGS` | `pending_auth_summary` (replatform candidate) |
| `…/cpy/CIPAUDTY.cpy` | `PA-AUTHORIZATION-KEY` (`PA-AUTH-DATE-9C S9(05) C3`, `PA-AUTH-TIME-9C S9(09) C3`), card, amounts, merchant, response, fraud flag | 200 (segment) | `PA-AUTHORIZATION-KEY` | C3 | IMS segment `PAUTDTL1` | same + `COPAUS2C` | `pending_auth_detail` |
| `…/cpy/CCPAURQY.cpy` | 18 fields `PA-RQ-*` (auth date/time, card, type, expiry, msg type/source, processing code, amount, MCC, country, POS mode, merchant id/name/city/state/ZIP, transaction id) | CSV text | — | — | MQ request body (comma-delimited, `UNSTRING`) | `COPAUA0C` | `messaging.md` §4.1 request JSON |
| `…/cpy/CCPAURLY.cpy` | `PA-RL-CARD-NUM X(16)`, `-TRANSACTION-ID X(15)`, `-AUTH-ID-CODE X(06)`, `-AUTH-RESP-CODE X(02)`, `-AUTH-RESP-REASON X(04)`, `-APPROVED-AMT +9(10).99` | CSV text | — | — | MQ reply body | `COPAUA0C` | `messaging.md` §4.2 reply JSON |
| `…/cpy/CCPAUERY.cpy` | `ERROR-LOG-RECORD` (date, time, app, program, location, level, subsystem, codes, message, event key) | 122 | — | — | error log written to TD queue `CSSL` | `COPAUA0C` | SQS `carddemo-error` / CloudWatch |
| `…/cpy/IMSFUNCS.cpy` | `FUNC-CODES` (DL/I function literals) | 40 | — | — | IMS call constants | `PAUDB*`, `DBUNLDGS` | not needed |
| `…/cpy/PAUTBPCB.CPY`, `PADFLPCB.CPY`, `PASFLPCB.CPY` | IMS PCB masks (DB / GSAM detail / GSAM summary) | 291/291/136 | — | C | PSB PCBs | `PAUDB*`, `DBUNLDGS` | not needed |
| `app/app-transaction-type-db2/cpy/CSDB2RWY.cpy` | DB2 common vars (`WS-DISP-SQLCODE`, `WS-DB2-CURRENT-ACTION`, `DSNTIAC` message area) | var | — | C3, C, R, O | — | `COTRTLIC` (`EXEC SQL INCLUDE`) | error envelope mapping |
| `…/cpy/CSDB2RPY.cpy` | PROCEDURE DIVISION: format SQL error via `DSNTIAC` | n/a | — | — | — | `COTRTLIC` | not needed |
| `app/app-transaction-type-db2/dcl/DCLTRTYP.dcl` / `DCLTRCAT.dcl` | DCLGEN for `TRANSACTION_TYPE` / `TRANSACTION_TYPE_CATEGORY` | — | `TR_TYPE` / (`TRC_TYPE_CODE`,`TRC_TYPE_CATEGORY`) | — | DB2 tables (`ddl/TRNTYPE.ddl`, `TRNTYCAT.ddl`, `XTRNTYPE.ddl`, `XTRNTYCAT.ddl`) | `COTRTLIC/UPC`, `COBTUPDT` | `transaction_type`, `transaction_category` (merged with VSAM, `data-model.md` §2.10–2.11) |
| `app/app-authorization-ims-db2-mq/dcl/AUTHFRDS.dcl` | DCLGEN for `AUTHFRDS` (`ddl/AUTHFRDS.ddl`, `ddl/XAUTHFRD.ddl`) | — | (`CARD_NUM`,`AUTH_TS`) | — | DB2 table | `COPAUS2C` | `authfrds` |
| `…/ims/DBPAUTP0.dbd`, `DBPAUTX0.dbd`, `PADFLDBD.DBD`, `PASFLDBD.DBD`, `PSBPAUTB.psb`, `PSBPAUTL.psb`, `PAUTBUNL.PSB`, `DLIGSAMP.PSB` | HIDAM DB (root `PAUTSUM0` 100 / child `PAUTDTL1` 200, key `ACCNTID`), index DB (`PAUTINDX`), GSAM files (200 / 100), PSBs | — | `ACCNTID` | — | IMS | CICS + BMP programs | `pending_auth_*` tables or replatform (§9) |

Symbolic BMS copybooks are listed with their maps in §5.

## 5. BMS maps and symbolic map copybooks

| Mapset (`.bms`) | Map | Symbolic copybook | Screen purpose (title from `.bms`) | Program | React route (`api.md` §9) | Owner |
|---|---|---|---|---|---|---|
| `app/bms/COSGN00.bms` | `COSGN0A` | `app/cpy-bms/COSGN00.CPY` | Signon (user id, password) | `COSGN00C` | `/login` | FE |
| `app/bms/COMEN01.bms` | `COMEN1A` | `app/cpy-bms/COMEN01.CPY` | Main Menu | `COMEN01C` | `/menu` | FE |
| `app/bms/COADM01.bms` | `COADM1A` | `app/cpy-bms/COADM01.CPY` | Admin Menu | `COADM01C` | `/admin` | FE |
| `app/bms/COACTVW.bms` | `CACTVWA` | `app/cpy-bms/COACTVW.CPY` | View Account | `COACTVWC` | `/accounts/view` | FE |
| `app/bms/COACTUP.bms` | `CACTUPA` | `app/cpy-bms/COACTUP.CPY` | Update Account | `COACTUPC` | `/accounts/update` | FE |
| `app/bms/COCRDLI.bms` | `CCRDLIA` | `app/cpy-bms/COCRDLI.CPY` | List Credit Cards | `COCRDLIC` | `/cards` | FE |
| `app/bms/COCRDSL.bms` | `CCRDSLA` | `app/cpy-bms/COCRDSL.CPY` | View Credit Card Detail | `COCRDSLC` | `/cards/view` | FE |
| `app/bms/COCRDUP.bms` | `CCRDUPA` | `app/cpy-bms/COCRDUP.CPY` | Update Credit Card Details | `COCRDUPC` | `/cards/update` | FE |
| `app/bms/COTRN00.bms` | `COTRN0A` | `app/cpy-bms/COTRN00.CPY` | List Transactions | `COTRN00C` | `/transactions` | FE |
| `app/bms/COTRN01.bms` | `COTRN1A` | `app/cpy-bms/COTRN01.CPY` | View Transaction | `COTRN01C` | `/transactions/view` | FE |
| `app/bms/COTRN02.bms` | `COTRN2A` | `app/cpy-bms/COTRN02.CPY` | Add Transaction | `COTRN02C` | `/transactions/new` | FE |
| `app/bms/CORPT00.bms` | `CORPT0A` | `app/cpy-bms/CORPT00.CPY` | Transaction Reports (monthly / yearly / custom) | `CORPT00C` | `/reports` | FE |
| `app/bms/COBIL00.bms` | `COBIL0A` | `app/cpy-bms/COBIL00.CPY` | Bill Payment | `COBIL00C` | `/bill-payment` | FE |
| `app/bms/COUSR00.bms` | `COUSR0A` | `app/cpy-bms/COUSR00.CPY` | List Users | `COUSR00C` | `/admin/users` | FE |
| `app/bms/COUSR01.bms` | `COUSR1A` | `app/cpy-bms/COUSR01.CPY` | Add User | `COUSR01C` | `/admin/users/new` | FE |
| `app/bms/COUSR02.bms` | `COUSR2A` | `app/cpy-bms/COUSR02.CPY` | Update User | `COUSR02C` | `/admin/users/:userId/edit` | FE |
| `app/bms/COUSR03.bms` | `COUSR3A` | `app/cpy-bms/COUSR03.CPY` | Delete User | `COUSR03C` | `/admin/users/:userId/delete` | FE |
| `app/app-authorization-ims-db2-mq/bms/COPAU00.bms` | `COPAU0A` | `…/cpy-bms/COPAU00.cpy` | View Authorizations (summary + 5 rows) | `COPAUS0C` | `/authorizations` | FE (conditional) |
| `app/app-authorization-ims-db2-mq/bms/COPAU01.bms` | `COPAU1A` | `…/cpy-bms/COPAU01.cpy` | View Authorization Details | `COPAUS1C` | `/authorizations/:acctId/:authKey` | FE (conditional) |
| `app/app-transaction-type-db2/bms/COTRTLI.bms` | `CTRTLIA` | `…/cpy-bms/COTRTLI.cpy` | Maintain Transaction Type (list/filter) | `COTRTLIC` | `/admin/transaction-types` | FE |
| `app/app-transaction-type-db2/bms/COTRTUP.bms` | `CTRTUPA` | `…/cpy-bms/COTRTUP.cpy` | Maintain Transaction Type (add/update) | `COTRTUPC` | `/admin/transaction-types/maintain` | FE |

## 6. Datasets and data files

### 6.1 VSAM clusters (from `app/jcl/*FILE.jcl`, `app/catlg/LISTCAT.txt`, `app/csd/CARDDEMO.CSD`)

| Dataset (`AWS.M2.CARDDEMO.` omitted) | Org | Key (len @ offset) | LRECL | Copybook | CICS file | Target |
|---|---|---|---|---|---|---|
| `USRSEC.VSAM.KSDS` | KSDS | 8 @ 0 | 80 | `CSUSR01Y` | `USRSEC` | `carddemo.user_security` |
| `ACCTDATA.VSAM.KSDS` | KSDS | 11 @ 0 | 300 | `CVACT01Y` | `ACCTDAT` | `carddemo.account` |
| `CARDDATA.VSAM.KSDS` | KSDS | 16 @ 0 | 150 | `CVACT02Y` | `CARDDAT` | `carddemo.card` |
| `CARDDATA.VSAM.AIX` (+ `.PATH`) | AIX, non-unique | 11 @ 16 | — | `CVACT02Y` | `CARDAIX` | index `ix_card_acct_id` |
| `CARDXREF.VSAM.KSDS` | KSDS | 16 @ 0 | 50 | `CVACT03Y` | `CCXREF` | `carddemo.card_xref` |
| `CARDXREF.VSAM.AIX` (+ `.PATH`) | AIX, non-unique | 11 @ 25 | — | `CVACT03Y` | `CXACAIX` | index `ix_card_xref_acct_id` |
| `CUSTDATA.VSAM.KSDS` | KSDS | 9 @ 0 | 500 | `CVCUS01Y` | `CUSTDAT` | `carddemo.customer` |
| `TRANSACT.VSAM.KSDS` | KSDS | 16 @ 0 | 350 | `CVTRA05Y` | `TRANSACT` | `carddemo.transaction` |
| `TRANSACT.VSAM.AIX` (+ `.PATH`) | AIX, non-unique | 26 @ 304 | — | `CVTRA05Y` | — | index `ix_transaction_proc_ts` |
| `TCATBALF.VSAM.KSDS` | KSDS | 17 @ 0 | 50 | `CVTRA01Y` | — | `carddemo.tran_cat_balance` |
| `DISCGRP.VSAM.KSDS` | KSDS | 16 @ 0 | 50 | `CVTRA02Y` | — | `carddemo.disclosure_group` |
| `TRANTYPE.VSAM.KSDS` | KSDS | 2 @ 0 | 60 | `CVTRA03Y` | — | `carddemo.transaction_type` |
| `TRANCATG.VSAM.KSDS` | KSDS | 6 @ 0 | 60 | `CVTRA04Y` | — | `carddemo.transaction_category` |
| `TRXFL.VSAM.KSDS` | KSDS (work) | 32 @ 0 | 350 | `COSTM01` | — | none (SQL sort in `create-statements`) |
| `EXPORT.DATA` | KSDS | 4 @ 28 | 500 | `CVEXPORT` | — | S3 `export/…` |
| `USRSEC.VSAM.ESDS` / `.RRDS` | ESDS / RRDS (demo, `ESDSRRDS.jcl`) | — / RRN | 80 | `CSUSR01Y` | — | not needed |
| CICS TD `JOBS` | extrapartition TDQ → JES INTRDR | — | 80 | — | `JOBS` | SQS `carddemo-report-request` |

GDG bases (§3.1 `DEFGDGB`/`DEFGDGD`/`REPTFILE`/`DALYREJS`): `TRANSACT.BKUP`, `TRANSACT.DALY`, `TRANREPT`,
`TCATBALF.BKUP`, `SYSTRAN`, `TRANSACT.COMBINED`, `DALYREJS`, `TRANTYPE.BKUP`, `TRANCATG.PS.BKUP`,
`DISCGRP.BKUP` → versioned S3 prefixes (`batch.md` §1.2).

### 6.2 Sample data files

Records = bytes ÷ LRECL (EBCDIC) or line count (ASCII). EBCDIC files are the fixed-width load source for
DM (code page / zoned-decimal rules in `data-model.md` §5); ASCII files are equivalent text copies.

| File | Org / LRECL | Records | Copybook | Target |
|---|---|---|---|---|
| `app/data/EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS` | PS FB 300 | 50 | `CVACT01Y` | `account` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.ACCDATA.PS` | PS FB 300 | 50 | `CVACT01Y` | byte-identical duplicate of `ACCTDATA.PS` (`cmp`), not referenced by any JCL → ignore |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.CARDDATA.PS` | PS FB 150 | 50 | `CVACT02Y` | `card` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.CARDXREF.PS` | PS FB 50 | 50 | `CVACT03Y` | `card_xref` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.CUSTDATA.PS` | PS FB 500 | 50 | `CVCUS01Y` | `customer` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS` | PS FB 350 | 300 | `CVTRA06Y` | S3 `input/dalytran/<businessDate>/dalytran.txt` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS.INIT` | PS FB 350 | 1 | `CVTRA05Y` | `TRANFILE.jcl` priming record: 350 bytes of low-values (dummy so CICS can open a non-empty KSDS) → **not loaded**, `transaction` starts empty |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.DISCGRP.PS` | PS FB 50 | 51 | `CVTRA02Y` | `disclosure_group` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS` | PS FB 500 | 500 | `CVEXPORT` | S3 `import/` test input for `import-customer-data` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.TCATBALF.PS` | PS FB 50 | 50 | `CVTRA01Y` | `tran_cat_balance` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.TRANCATG.PS` | PS FB 60 | 18 | `CVTRA04Y` | `transaction_category` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.TRANTYPE.PS` | PS FB 60 | 7 | `CVTRA03Y` | `transaction_type` |
| `app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS` | PS FB 80 | 10 | `CSUSR01Y` | `user_security` (hash passwords) |
| `app/data/EBCDIC/.gitkeep` | — | — | — | placeholder, ignore |
| `app/data/ASCII/acctdata.txt`, `carddata.txt`, `cardxref.txt`, `custdata.txt`, `dailytran.txt`, `discgrp.txt`, `tcatbal.txt`, `trancatg.txt`, `trantype.txt` | text, 1 record/line | 50, 50, 50, 50, 300, 51, 50, 18, 7 | as EBCDIC counterparts | same tables (ASCII has no `USRSEC`/`EXPORT` copy) |
| `app/app-authorization-ims-db2-mq/data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat` | IMS unload (51,736 bytes) | — | `CIPAUSMY`/`CIPAUDTY` | `pending_auth_summary` (21 roots + 1 all-spaces terminator, skipped) / `pending_auth_detail` (202); tables in `aws/db/ims/`, loaded by DM so the data is ready if IMS is refactored |

### 6.3 DB2 and IMS

| Object | Source | Key | Target |
|---|---|---|---|
| DB2 `CARDDEMO.AUTHFRDS` (+ index `XAUTHFRD`) | `app/app-authorization-ims-db2-mq/ddl/AUTHFRDS.ddl`, `XAUTHFRD.ddl` | (`CARD_NUM`, `AUTH_TS`) | `carddemo.authfrds` |
| DB2 `CARDDEMO.TRANSACTION_TYPE` (+ `XTRNTYPE`) | `app/app-transaction-type-db2/ddl/TRNTYPE.ddl`, `XTRNTYPE.ddl` | `TR_TYPE` | `carddemo.transaction_type` |
| DB2 `CARDDEMO.TRANSACTION_TYPE_CATEGORY` (+ `XTRNTYCAT`) | `…/ddl/TRNTYCAT.ddl`, `XTRNTYCAT.ddl` | (`TRC_TYPE_CODE`, `TRC_TYPE_CATEGORY`) | `carddemo.transaction_category` |
| IMS HIDAM `DBPAUTP0` (root `PAUTSUM0`, child `PAUTDTL1`) + index `DBPAUTX0` | `app/app-authorization-ims-db2-mq/ims/*.dbd` | `ACCNTID` | `pending_auth_summary` / `pending_auth_detail` (**replatform candidate**) |
| IMS GSAM `PADFLDBD` (200) / `PASFLDBD` (100) | `…/ims/PADFLDBD.DBD`, `PASFLDBD.DBD` | — | not needed (unload targets) |

## 7. MQ usage → SQS

| Program | Legacy queue(s) | Format | SQS (`messaging.md`) |
|---|---|---|---|
| `COPAUA0C` | request: trigger `MQTM-QNAME` (= `AWS.M2.CARDDEMO.PAUTH.REQUEST` per `app/app-authorization-ims-db2-mq/README.md`); reply: request `MQMD-REPLYTOQ` (= `AWS.M2.CARDDEMO.PAUTH.REPLY`) via `MQPUT1`; errors: TD `CSSL` | CSV `CCPAURQY` (18 fields) → CSV `CCPAURLY` (6 fields); correl id preserved; nonpersistent; expiry 50; up to 500 msgs per trigger, 5000 ms wait | `carddemo-pauth-request` → `carddemo-pauth-reply` (+ DLQs); errors → `carddemo-error` (§4, §5) |
| `COACCT01` | request: trigger `MQTM-QNAME`; reply `CARD.DEMO.REPLY.ACCT`; error `CARD.DEMO.ERROR` | `WS-FUNC X(04)` + `WS-KEY 9(11)` + filler (1000) → formatted account fields | `carddemo-acct-inquiry-request` → `carddemo-acct-inquiry-reply`; `carddemo-error` (§3.1–3.2) |
| `CODATE01` | request: trigger `MQTM-QNAME`; reply `CARD.DEMO.REPLY.DATE`; error `CARD.DEMO.ERROR` | any request → `SYSTEM DATE : MM-DD-YYYY SYSTEM TIME : HH:MM:SS` | `carddemo-date-inquiry-request` → `carddemo-date-inquiry-reply` (§3.3–3.4) |
| `CORPT00C` (not MQ) | CICS TD `JOBS` | 80-byte JCL cards | `carddemo-report-request` (§6) |

## 8. Owning-session summary

| Session | Scope from this inventory |
|---|---|
| DM | §6 all tables + indexes (Flyway DDL from `data-model.md`), EBCDIC/ASCII loaders for §6.2, DB2 seed (`CREADB21`), optional IMS unload → `pending_auth_*`, reconciliation counts (§6.2 record counts) |
| ON | §1 all programs → `com.carddemo.*` services + `api.md`; SQS consumers for `COACCT01`, `CODATE01`, (`COPAUA0C` if refactored); `CSUTLDTC` validator |
| BA | §2 programs + §3 JCL targets → Spring Batch jobs and Step Functions (`batch.md`) |
| FE | §5 all maps → routes in `api.md` §9 |
| IN | Aurora, S3 bucket/prefixes + lifecycle (GDG replacement), SQS queues + DLQs, AWS Batch, Step Functions, EventBridge schedules, ECS for services, Secrets Manager |
| VA | Parity: record counts §6.2, `CBTRN02C` reject reasons, interest formula, API messages; confirms replatform list §9 |

## 9. Replatform candidates and incomplete modules (fallback rule)

These are **not** faked by the refactor sessions. Each is either replatformed on the AWS Mainframe
Modernization runtime (prebuilt packages in `samples/m2/`) or explicitly left incomplete, and must be listed
in each owning session's PR description.

| Module | Source | Decision | Rationale |
|---|---|---|---|
| IMS HIDAM pending-authorization DB (`DBPAUTP0`/`DBPAUTX0`) and its programs `COPAUA0C`, `COPAUS0C`, `COPAUS1C`, `CBPAUP0C`, `PAUDBUNL`, `PAUDBLOD`, `DBUNLDGS` | `app/app-authorization-ims-db2-mq/` | **Replatform candidate** — confirmed by ON (`aws/services/` does not implement `COPAUA0C`, the `carddemo-pauth-*` consumer, or `/api/v1/authorizations`); the whole sub-app stays on M2 runtime unless a later session refactors it onto `pending_auth_*`. **DM:** tables created (`aws/db/ims/pending_auth.sql`) and the `DBPAUTP0` unload is decoded and loaded (21 summaries / 202 details) | Hierarchical DL/I navigation (`GU`/`GNP`/`ISRT`/`REPL`/`DLET`), `COMP-3` keys built from date/time complements, `CHKP` restart logic in BMP, combined MQ + IMS + VSAM + DB2 unit of work with `SYNCPOINT`; no test harness in repo |
| `COPAUS2C` + DB2 `AUTHFRDS` | same | **Replatform candidate** with the IMS module (not refactored by ON) | Only reachable from `COPAUS1C` |
| Assembler `COBDATFT` | `app/asm/COBDATFT.asm`, `app/maclib/COCDATFT.mac` | Not ported as code; behaviour re-implemented with `java.time` in `extract-accounts` | 370 assembler; only used by `CBACT01C` for date re-formatting |
| Assembler `MVSWAIT` + `COBSWAIT` | `app/asm/MVSWAIT.asm`, `app/cbl/COBSWAIT.cbl` | Not ported; Step Functions `Wait` | Pure timing utility; existed only to space out jobs |
| `CBACT04C` paragraph `1400-COMPUTE-FEES` | `app/cbl/CBACT04C.cbl` | **Incomplete in source** ("To be implemented"); `calculate-interest` implements interest only, fees left as a documented no-op | Nothing to migrate; inventing fee rules would be fabrication |
| `TXT2PDF` REXX | `app/jcl/TXT2PDF1.JCL` (`AWS.M2.LBD.TXT2PDF.EXEC`, external library not in repo) | Re-implemented with a Java PDF library in `statement-pdf`; byte-level PDF parity not required | Source of the REXX exec is not in the repository |
| CSD transaction `CDV1` → `COCRDSEC` | `app/csd/CARDDEMO.CSD` | Not migrated | Program source absent from repo |
| Batch demonstration / on-demand jobs `extract-accounts`, `print-cards`, `print-xref`, `print-customers`, `export-customer-data`, `import-customer-data`, `PRTCATBL` category-balance report | `app/jcl/READ*.jcl`, `CBEXPORT.jcl`, `CBIMPORT.jcl`, `PRTCATBL.jcl` (`CBACT01C`–`03C`, `CBCUS01C`, `CBEXPORT`, `CBIMPORT`) | **Left incomplete** by the batch session (`aws/batch/` v1); refactor later (low risk) | Not part of any scheduled chain (on demand only); the daily/weekly/monthly cycle is complete without them |
| DB2 `COTRTLIC` / `COTRTUPC` (1,861 / 1,429 LOC) | `app/app-transaction-type-db2/cbl/` | **Refactored (ON)** → `aws/services/` `/api/v1/transaction-types` (api.md §10.2) | Plain SQL CRUD; no replatforming needed |
| MQ `COACCT01` / `CODATE01` | `app/app-vsam-mq/` | **Refactored (ON)** → `aws/services/` `SqsInquiryConsumer` (off by default, `CARDDEMO_MESSAGING_ENABLED=true`) | Request/reply over `carddemo-acct-inquiry-*` / `carddemo-date-inquiry-*`; not exercised against real SQS in the ON session |
| Frontend screens for optional sub-apps: `COPAU00`/`COPAU01` (`/authorizations…`) and `COTRTLI`/`COTRTUP` (`/admin/transaction-types…`) | `app/app-authorization-ims-db2-mq/bms/`, `app/app-transaction-type-db2/bms/` | **Not built** in `aws/frontend/` (FE). Menu options COMEN01 #11 and COADM01 #5–#6 are listed but flagged *not installed* and return the legacy "This option … is not installed" message | Follows the backing module decisions above: authorization sub-app defaults to replatform; transaction-type screens to be added once ON ships `/api/v1/transaction-types` |

## 10. Coverage checklist

Counts produced with `ls`/`find` on the listed directories; every file appears in the section shown.

| Directory | Files | Covered in | Status |
|---|---|---|---|
| `app/cbl/` (`CO*` online 17 + `COBSWAIT` + `CB*` 12 + `CSUTLDTC`) | 31 | §1.1 (17), §2 (14) | ✔ 31/31 |
| `app/cpy/` | 30 | §4.1 | ✔ 30/30 |
| `app/bms/` | 17 | §5 | ✔ 17/17 |
| `app/cpy-bms/` | 17 | §5 | ✔ 17/17 |
| `app/jcl/` | 38 | §3.1 | ✔ 38/38 |
| `app/data/EBCDIC/` | 13 data + `.gitkeep` | §6.2 | ✔ 14/14 |
| `app/data/ASCII/` | 9 | §6.2 | ✔ 9/9 |
| `app/csd/` | 1 | §0, §1, §3.3 | ✔ |
| `app/asm/`, `app/maclib/` | 2 + 2 | §2, §3.3 | ✔ |
| `app/proc/`, `app/ctl/`, `app/catlg/` | 2 + 1 + 1 | §3.3 | ✔ |
| `app/scheduler/` | 2 | §3.4 | ✔ |
| `app/app-authorization-ims-db2-mq/` — cbl 8, cpy 9, cpy-bms 2, bms 2, jcl 5, ims 8, ddl 2, dcl 1, csd 1, data 1, README 1 | 40 | §1.2, §2, §3.2, §4.2, §5, §6, §7, §9 | ✔ 40/40 |
| `app/app-transaction-type-db2/` — cbl 3, cpy 2, cpy-bms 2, bms 2, jcl 3, ctl 7, ddl 4, dcl 2, csd 1, README 1 | 27 | §1.2, §2, §3.2, §3.3, §4.2, §5, §6.3 | ✔ 27/27 |
| `app/app-vsam-mq/` — cbl 2, csd 1, README 1 | 4 | §1.2, §7 | ✔ 4/4 |
| `samples/jcl/`, `samples/proc/`, `samples/m2/` | 9 + 4 + 2 | §3.3 | ✔ 15/15 |

Transaction IDs: 25 `DEFINE TRANSACTION` across the four CSD files (18 in `CARDDEMO.CSD` incl. `CDV1`; 3 in
`CRDDEMO2.csd`: `CP00`, `CPVS`, `CPVD`; 2 in `CRDDEMOD.csd`: `CTLI`, `CTTU`; 2 in `CRDDEMOM.csd`: `CDRA`, `CDRD`),
all mapped in §1 except `CDV1` (§9).
