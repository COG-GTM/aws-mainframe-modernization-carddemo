# JCL → Java job mapping

Every member of `app/jcl` (38) and `app/proc` (2), and what replaces it in `modernization/`.
Sources: the JCL itself, `inventory.md` §7–§8, `app/scheduler/CardDemo.controlm` and `CardDemo.ca7`.

## How a JCL job runs in Java

| JCL concept | Java equivalent |
|---|---|
| `//POSTTRAN JOB` + `EXEC PGM=` | A Spring Batch job registered under the JCL member name in a service's `BatchJobRegistry` (`com.carddemo.common.batch`). Launched with `java -jar <service>.jar --job=POSTTRAN [--date=2022-07-18]` (exits with the job's return code) or over HTTP (`POST /api/v1/batch/...`, transaction-service). |
| Input DD (`DALYTRAN DD DSN=...`) | `--dd=DALYTRAN=<file>`: a fixed-width file read with `RecordReader` (`*.txt` = line sequential, as in `app/data/ASCII`). Without the DD, the job reads the service's tables, which the data-loader filled from the same file. |
| Output DD, GDG `(+1)` | A file named after the DD in a new generation directory `<output-dir>/<JOB>/<yyyyMMdd-HHmmss-SSS>/` written with `RecordWriter` (`--codepage=ebcdic`, `--format=line` to change the encoding / organization). |
| GDG `(0)` input | The same DD in the latest generation directory of the producing job (e.g. COMBTRAN's `SYSTRAN(0)` = newest `out/INTCALC/*/SYSTRAN`), or `--dd=` to name a file. |
| `PARM='2022071800'` | `--parm=2022071800` → job parameter `parm`; defaults to `<--date as yyyyMMdd>00`. |
| `DATEPARM DD` (TRANREPT) | `--date` and `--parm` job parameters (`run.date`, `parm`). The TRANREPT start/end dates become job parameters when that job is built (UNT149-14). |
| Step return code, `RETURN-CODE` | `ReturnCodes.set(...)` per step; the job RC is the highest step RC, 16 when a step abends. Written to `<generation>/RETURN-CODE` and used as the CLI exit code. |
| `COND=(0,NE)` | `CondTasklet` + `JclCond`: a step is bypassed (status `NOOP`, `jcl.bypassed`) when the condition is true for any earlier step RC. An abend fails the job, so later steps do not run and the exit code is 16. |
| `SORT FIELDS=`, `INCLUDE COND=`, `OUTREC FIELDS=`, concatenated `SORTIN` | `RecordSort`: stable byte-order sort of fixed-width records (EBCDIC order when the data is EBCDIC), include predicate, OUTREC field moves, several SORTIN lists concatenated. |
| IDCAMS DEFINE / REPRO, IEBGENER, IEFBR14 data-set setup | Data-definition only: schema by Flyway migrations in each service, contents by the `data-loader` module (`app/data` → the five schemas). |
| CICS `CEMT SET FIL CLO/OPE`, `COBSWAIT` | Not needed: services share PostgreSQL with transactional isolation; no file is closed for batch. |

Return codes of the CLI itself: 12 for a usage error (unknown option, bad date), 16 for an
abended job, otherwise the job's RC (POSTTRAN ends 4 when it rejected transactions, as CBTRN02C does).

## Jobs

Status: **built** = runs today; **planned** = the Java job does not exist yet, target given;
**data-definition only** = replaced by Flyway/loader; **not migrated** = no Java counterpart, reason given.

### Business batch

| JCL | Steps | Java job | Status |
|---|---|---|---|
| POSTTRAN | STEP15 CBTRN02C | transaction-service `POSTTRAN` (`postTransactionsJob`): `stageDailyTransactionsStep` (DD DALYTRAN file → `daily_transactions`), `postTransactionsStep` (`PostingService`, RC 4 on rejects), `writePostingOutputsStep` (DALYREJS 430-byte rejects of the run, TRANSACT and TCATBALF after the run). ACCTFILE stays in account-service, whose posting endpoint is idempotent per DALYTRAN-ID (`account.account_postings`). | built; matches GnuCOBOL CBTRN02C on the sample DALYTRAN (UNT149-12) |
| (none) | CBTRN01C | transaction-service `CBTRN01C` (`verifyTransactionsJob`, keyed by program name since no JCL member runs it): `verifyDailyTransactionsStep` reads DD DALYTRAN (required), looks up each card and account and writes the program's DISPLAY lines to `SYSOUT`; no updates, RC 0. | built (UNT149-12) |
| INTCALC | STEP15 CBACT04C PARM='2022071800' | transaction-service `INTCALC` (`interestCalculationJob`): `interestCalculationStep` (`InterestCalculationService`; TRAN-ID = PARM-DATE + 6-digit run sequence, as CBACT04C), `writeSystemTransactionsStep` (SYSTRAN(+1): the run's TRAN-IDs, by TRAN-ID). A PARM whose TRAN-IDs are already in `transaction.transactions` abends the run (RC 16) instead of overwriting them. CBACT04C never rewrites the **last** TCATBAL account (the `PERFORM UNTIL END-OF-FILE = 'Y'` test runs first, so the `ELSE PERFORM 1050-UPDATE-ACCOUNT` branch is dead): its interest transactions are written but ACCT-CURR-BAL and the cycle totals stay as they were. The Java job keeps this COBOL defect for parity. | built; byte-identical SYSTRAN + ACCTDATA vs CBACT04C (`IntcalcCobolParityTest`, `scripts/migration/intcalc-reference/run.py`) |
| COMBTRAN | STEP05R SORT; STEP10 IDCAMS REPRO | transaction-service `COMBTRAN` (`combineTransactionsJob`): `combineTransactionsSortStep` sorts TRANSACT.BKUP(0)+SYSTRAN(0) by TRAN-ID → TRANSACT.COMBINED. STEP10 (load into TRANSACT KSDS) is not needed: INTCALC already inserts into `transaction.transactions`. | built (sort); TRANSACT.BKUP(0) must be given with `--dd` until TRANBKP has a Java job |
| TRANREPT | STEP05R REPROC; STEP05R SORT INCLUDE on dates; STEP10R CBTRN03C | batch-service `TRANREPT`: `RecordSort` with an INCLUDE on TRAN-PROC-DT between the DATEPARM dates, then the report program writing TRANREPT(+1). Also submitted online by CORPT00C (`EXEC PROC=TRANREPT`). | planned, UNT149-14 |
| CREASTMT | DELDEF01 IDCAMS; STEP010 SORT; STEP020 REPRO; STEP030 IEFBR14; STEP040 CBSTM03A | batch-service `CREASTMT`: STEP010 = `RecordSort` by card number + TRAN-ID with OUTREC (the framework sort is tested with this key in `RecordSortTest`); STEP020/030 data-definition only; STEP040 = CBSTM03A/B writing STMTFILE and HTMLFILE. | planned, UNT149-15 |
| CBEXPORT | STEP01 IDCAMS define; STEP02 CBEXPORT | batch-service `CBEXPORT` (EXPFILE written via `RecordWriter`). | planned, UNT149-15 |
| CBIMPORT | STEP01 CBIMPORT | batch-service `CBIMPORT` (EXPFILE read via `RecordReader`; CUSTOUT/ACCTOUT/XREFOUT/TRNXOUT/ERROUT files). | planned, UNT149-15 |
| READACCT | PREDEL IEFBR14; STEP05 CBACT01C | batch-service `READACCT` (OUTFILE/ARRYFILE/VBRCFILE). | planned, UNT149-15 |
| READCARD | STEP05 CBACT02C | batch-service `READCARD`. | planned, UNT149-15 |
| READCUST | STEP05 CBCUS01C | batch-service `READCUST`. | planned, UNT149-15 |
| READXREF | STEP05 CBACT03C | batch-service `READXREF`. | planned, UNT149-15 |
| PRTCATBL | DELDEF IEFBR14; STEP05R REPROC; STEP10R SORT | A `RecordSort` job over TCATBALF (category-balance print). | planned (no ticket owns it yet) |
| TRANBKP | STEP05R REPROC; STEP05/STEP10 IDCAMS delete/define | Backup of `transaction.transactions` as TRANSACT.BKUP(+1) (`RecordWriter` of the table, as POSTTRAN's TRANSACT DD). The delete/define part is data-definition only. | planned (no ticket owns it yet) |
| TXT2PDF1 | IKJEFT1B REXX TXT2PDF | Not migrated: PDF rendering of STATEMNT.PS is a presentation step; CBSTM03A's HTMLFILE covers the readable statement. | not migrated |

### Data-definition only, replaced by Flyway/loader

| JCL | Steps | Replaced by |
|---|---|---|
| ACCTFILE | IDCAMS delete/define/REPRO | account-service Flyway; data-loader loads `acctdata.txt` |
| CARDFILE | SDSF CEMT; IDCAMS delete/define/REPRO, AIX, PATH, BLDINDEX | card-service Flyway (index on account id = AIX); data-loader `carddata.txt` |
| XREFFILE | IDCAMS delete/define/REPRO, AIX, PATH, BLDINDEX | card-service Flyway (`card_xref` + account index); data-loader `cardxref.txt` |
| CUSTFILE | SDSF CEMT; IDCAMS delete/define/REPRO | customer-service Flyway; data-loader `custdata.txt` |
| TRANFILE | SDSF CEMT; IDCAMS delete/define/REPRO, AIX, PATH, BLDINDEX | transaction-service Flyway (`transactions`, indexes). The loader does not copy DALYTRAN.PS.INIT into `transactions`; it loads `dailytran.txt` into `daily_transactions`, which POSTTRAN posts. |
| TRANIDX | IDCAMS AIX, PATH, BLDINDEX | transaction-service Flyway indexes |
| TCATBALF | IDCAMS delete/define/REPRO | transaction-service Flyway; data-loader `tcatbal.txt` |
| DISCGRP | IDCAMS delete/define/REPRO | transaction-service Flyway; data-loader `discgrp.txt` |
| TRANTYPE | IDCAMS delete/define/REPRO | transaction-service Flyway; data-loader `trantype.txt` |
| TRANCATG | IDCAMS delete/define/REPRO | transaction-service Flyway; data-loader `trancatg.txt` |
| DUSRSECJ | IEFBR14; IEBGENER in-stream users; IDCAMS define/REPRO | auth-service Flyway; data-loader loads EBCDIC `USRSEC.PS` into `auth.users` |
| ESDSRRDS | IEFBR14; IEBGENER; IDCAMS ESDS/RRDS define/REPRO | none needed: ESDS/RRDS demonstration with no program reading it |
| DEFCUST | IDCAMS delete/define | customer-service Flyway (alternate cluster not used by any program) |
| DEFGDGB | IDCAMS define GDG bases | generation directories created per run by `JobRunDirectory` |
| DEFGDGD | IDCAMS define GDG; IEBGENER first generation | reference data loaded by data-loader; generations by `JobRunDirectory` |
| DALYREJS | IDCAMS define GDG | `out/POSTTRAN/<ts>/DALYREJS` |
| REPTFILE | IDCAMS define GDG | `out/TRANREPT/<ts>/TRANREPT` (when built) |

### Operational and utility jobs, not migrated

| JCL | What it does | Why no Java job |
|---|---|---|
| CLOSEFIL | CEMT close CICS files before batch | no file sharing between online and batch to coordinate |
| OPENFIL | CEMT re-open CICS files | same |
| WAITSTEP | COBSWAIT pause | scheduler-level delay, not needed |
| CBADMCDJ | DFHCSDUP CICS resource definitions | CICS-only; resource names differ from `CARDDEMO.CSD` (inventory hypothesis) |
| FTPJCL | FTP template | file transfer is outside the services |
| INTRDRJ1 | IDCAMS REPRO; IEBGENER to the internal reader | internal-reader chaining demo; the scheduler chains jobs |
| INTRDRJ2 | IDCAMS REPRO | second job of that demo |

### Procedures

| Proc | Java equivalent |
|---|---|
| REPROC.prc | Generic IDCAMS REPRO unload: a `RecordWriter` dump of the table (as `writePostingOutputsStep` does for TRANSACT/TCATBALF) or a `--dd` file read; no separate Java proc. |
| TRANREPT.prc | Same steps as TRANREPT.jcl → batch-service `TRANREPT` (planned, UNT149-14). |

## Schedule (Control-M `CardDemo.controlm`)

Folder order, chained by IN/OUT conditions. With the Java jobs, a scheduler runs the
non-data-definition jobs in the same order (CLI or HTTP launch); CLOSEFIL/WAITSTEP/OPENFIL drop out.

| Control-M folder | Mainframe order | Java order |
|---|---|---|
| DAILY-TransactionBackup (DAYS=ALL) | CLOSEFIL → TRANBKP → WAITSTEP → OPENFIL | TRANBKP (planned) |
| WEEKLY-TransactionTypesDBRefresh (DAYS=SA) | MNTTRDB2 → TRANEXTR | DB2 extension jobs, not in `app/jcl`; out of scope |
| WEEKLY-DisclosureGroupsRefresh (DAYS=SA, after MNTTRDB2) | CLOSEFIL → DISCGRP → WAITSTEP → OPENFIL | data-loader reload of `discgrp.txt` (data-definition only) |
| MONTHLY-InterestCalculation | CLOSEFIL → INTCALC → COMBTRAN → WAITSTEP → OPENFIL | `--job=INTCALC` → `--job=COMBTRAN` (COMBTRAN reads INTCALC's latest SYSTRAN) |

CA-7 (`CardDemo.ca7`) adds the chains Control-M does not carry: CLOSEFIL → CBPAUP0J → POSTTRAN →
WAITSTEP → OPENFIL (Java: `--job=POSTTRAN`); TRANTYPE / TRANCATG / TCATBALF loads (data-loader);
READACCT → READCARD → READCUST → READXREF; CREASTMT → TXT2PDF1; PRTCATBL. Neither schedule runs
TRANREPT (online, CORPT00C), CBEXPORT/CBIMPORT, or the file-define jobs.

Count check: 14 business + 17 data-definition + 7 operational = 38 JCL members; 2 procedures.
