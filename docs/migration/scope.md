# CardDemo migration: scope and definition of done

Ticket UNT149-3 (phase: discovery and scope). Inputs: `docs/migration/inventory.md` (program,
dataset and call-graph inventory) and `docs/migration/prior-work.md` (assessment of the earlier
`devin/*` branches). Anything marked **Hypothesis** is an inference, not something read in the
source. The COBOL under `app/` is the read-only reference and is not changed.

## 1. Scope decision

**Decision d-scope: core app, online and batch.** The migration covers the core VSAM/CICS
application under `app/`: all 31 programs in `app/cbl` and the copybooks, BMS maps, JCL, datasets
and sample data they use. Online and batch are done together because they share the same VSAM
files (inventory §9), so one half alone leaves little that can be checked end to end. Batch-only
is still the cheaper pilot if time gets short.

**Target.** The Java application continues `origin/devin/1789660764-carddemo-java-modernization`
(five Spring Boot services and PostgreSQL under `modernization/`, prior-work §1). Neither `main`
nor `migration/carddemo-java` has a `modernization/` tree yet, so the first Java ticket brings that
branch in.

**Reference for equivalence.** The COBOL on `main`, run with GnuCOBOL 3.1.2 on the sample data in
`app/data`, is the reference for batch programs. The 40 `devin/sfind-*` security fixes are not part
of the reference (prior-work §3.6).

## 2. In scope

Sizes are non-comment source lines (blank lines included).

### 2.1 Online CICS programs (17, 12,815 lines)

Each becomes a REST endpoint in one of the five services. The BMS map defines the fields and the
edit messages that endpoint has to match. The menu programs become routing in the client.

| Program | Tran | Lines | Target service | Status on the Java base branch |
|---|---|---|---|---|
| COSGN00C | CC00 | 211 | auth-service | ported |
| COMEN01C | CM00 | 255 | client routing (`nextScreen`) | ported |
| COADM01C | CA00 | 230 | client routing (`nextScreen`) | ported |
| COACTVWC | CAVW | 811 | account-service | ported |
| COACTUPC | CAUP | 3,744 | account-service | ported |
| COCRDLIC | CCLI | 1,256 | card-service | ported |
| COCRDSLC | CCDL | 757 | card-service | ported |
| COCRDUPC | CCUP | 1,365 | card-service | ported |
| COTRN00C | CT00 | 617 | transaction-service | ported |
| COTRN01C | CT01 | 273 | transaction-service | ported |
| COTRN02C | CT02 | 698 | transaction-service | ported |
| CORPT00C | CR00 | 586 | transaction-service (starts the CBTRN03C equivalent) | not built |
| COBIL00C | CB00 | 493 | transaction-service | ported |
| COUSR00C | CU00 | 615 | auth-service | ported |
| COUSR01C | CU01 | 238 | auth-service | ported |
| COUSR02C | CU02 | 351 | auth-service | ported |
| COUSR03C | CU03 | 296 | auth-service | ported |

"Ported" is the base branch's own label (`modernization/docs/COBOL-PROGRAM-MAPPING.md`). It is
backed by unit tests and code reading only; nothing on that branch has been compared with COBOL
output (prior-work §1). Every row still has to meet the definition of done in §4.

### 2.2 Batch programs and utilities (14, 5,239 lines)

| Program | Job | Lines | GnuCOBOL syntax | Status on the Java base branch |
|---|---|---|---|---|
| CBTRN02C | POSTTRAN | 672 | OK | ported (Spring Batch) |
| CBACT04C | INTCALC | 599 | OK | ported (Spring Batch) |
| CBTRN03C | TRANREPT | 596 | OK | built (batch-service, UNT149-14) |
| CBSTM03A | CREASTMT | 874 | compile shim (§5) | not built |
| CBSTM03B | subroutine of CBSTM03A | 204 | OK | not built |
| CBEXPORT | CBEXPORT | 491 | compile shim (§5) | not built |
| CBIMPORT | CBIMPORT | 413 | compile shim (§5) | not built |
| CBTRN01C | none (no JCL in `app/jcl` runs it) | 449 | OK | not built |
| CBACT01C | READACCT | 379 | OK | replaced by list endpoints |
| CBACT02C | READCARD | 144 | OK | replaced by list endpoints |
| CBACT03C | READXREF | 145 | OK | replaced by list endpoints |
| CBCUS01C | READCUST | 145 | OK | replaced by list endpoints |
| CSUTLDTC | subroutine (CORPT00C, COTRN02C, COACTUPC) | 128 | OK | no separate module; ported as a shared Java date check (replaces the LE `CEEDAYS` call) |
| COBSWAIT | WAITSTEP | 19 | OK | replaced: it only waits between jobs, job ordering does that (§3) |

CBACT01C–CBACT03C and CBCUS01C print or write file dumps. The base branch replaces them with
`GET` list endpoints. That is only accepted if the dump files they write are produced again (§4.2);
otherwise the replacement is recorded as a deliberate deviation.

### 2.3 Data and job flow

- **Datasets:** the ten master/reference KSDS (ACCTDATA, CARDDATA + AIX, CARDXREF + AIX, CUSTDATA,
  TRANSACT, USRSEC, TCATBALF, DISCGRP, TRANTYPE, TRANCATG), the working files TRXFL and EXPORT.DATA,
  and the sequential and GDG files the batch programs read and write (inventory §6). They become
  PostgreSQL tables or files according to the copybook layouts (inventory §4).
- **Sample data:** `app/data/EBCDIC` (13 files) and `app/data/ASCII` (9 files) are loaded into the
  Java side and are the test data for every equivalence check.
- **Job flow:** the utility steps whose output feeds a program are ported as job steps: IDCAMS
  REPRO loads and unloads, and the SORT steps in COMBTRAN, TRANREPT, CREASTMT and PRTCATBL. The
  file-define jobs (IDCAMS DEFINE, GDG bases) become schema migrations.

## 3. Out of scope

| Item | Why / what replaces it |
|---|---|
| `app/app-authorization-ims-db2-mq`, `app/app-transaction-type-db2`, `app/app-vsam-mq` | Extensions (IMS, DB2, MQ), excluded by d-scope. Menu option 11 (`COPAUS0C`) and admin options 5–6 (`COTRTLIC`, `COTRTUPC`) point here; in Java they return "not available". The extension job `CBPAUP0J` that CA-7 runs before POSTTRAN is dropped from the job order. |
| CSD transaction `CDV1` → `COCRDSEC` | Defined in `CARDDEMO.CSD`, but there is no source in the repository. |
| Assembler `COBDATFT`, `MVSWAIT` (+ `app/maclib`) | Replaced, not ported. `COBDATFT` (date formatting for CBACT01C) becomes Java date formatting, checked through CBACT01C's output. `MVSWAIT` is only called by COBSWAIT, which is replaced. |
| Control-M (`CardDemo.controlm`) and CA-7 (`CardDemo.ca7`) | Replaced by an explicit job order in the Java batch runner, taken from the chains in inventory §8 (minus the extension jobs). Calendars and IN/OUT conditions are not ported. |
| CICS file open/close steps (CLOSEFIL, OPENFIL, the SDSF `CEMT` steps in the load jobs) | Not needed: online and batch share one database. |
| TXT2PDF1 (REXX `TXT2PDF`) | The statement text and HTML from CBSTM03A are in scope; turning them into PDF is not. |
| Demonstration and template jobs: ESDSRRDS, DEFCUST, FTPJCL, INTRDRJ1/2, CBADMCDJ | Not used by any program; CBADMCDJ uses resource names that do not match `CARDDEMO.CSD`. |
| The `devin/sfind-*` COBOL security fixes | Not in the reference. Security changes made in Java (password hashing, sign-on token, role checks) are allowed but each one is recorded as a deliberate deviation and left out of the equivalence comparison. |

## 4. Definition of done (per program)

A program is done when its Java replacement is merged on `migration/carddemo-java`, the build passes
(`cd modernization && JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify`), and its output
is shown to be equivalent on the sample data, as below. The evidence (command and output) goes in
the PR.

### 4.1 Batch programs

1. A golden set exists for the program: input files built from `app/data` (plus edge-case records
   where the source has branches the sample data does not reach), and the expected outputs from
   running the unmodified COBOL with GnuCOBOL. The harness on `devin/1789609454-golden-set-harness`
   and the equivalence runner on `devin/1788437007-posttran-modernization` are reused for this
   (prior-work §2).
2. The Java job runs on the same inputs and its outputs match field by field: every output file and
   every updated master record, plus the return code (for example 0 / 4 for CBTRN02C).
3. Every difference is either fixed or listed as a deliberate deviation with a reason (for example
   the new `TRAN-ID` scheme).

### 4.2 File-dump programs (CBACT01C, CBACT02C, CBACT03C, CBCUS01C)

Same as §4.1 for the files they write (CBACT01C writes three output files). For the ones that only
`DISPLAY` records, the Java side has to return the same records in the same order and with the same
field values.

### 4.3 Online programs

CICS programs cannot be run on this VM (no CICS translator or runtime), so there is no executable
COBOL reference for them. For each program:

1. A scenario list written from the source: for every path through the program (valid input, each
   edit error, not found, PF-key exits), the input, the expected message text from the source, and
   the expected changes to the data. Expected values are computed from the sample data.
2. An automated API test per scenario on the Java side that passes.
3. Where the program changes data (COACTUPC, COCRDUPC, COTRN02C, COBIL00C, COUSR01C–03C), the
   resulting rows match the record the COBOL would write, field by field.

**Hypothesis:** this is weaker evidence than the batch golden sets, because the expected results come
from reading the COBOL, not from running it. Running the online programs in a rehosting runtime
(the `samples/m2` and `samples/m2/unikix` setups in this repository) would close that gap; it was
not tried here.

## 5. CBEXPORT, CBIMPORT, CBSTM03A: verified through compile shims

These three programs do not pass `cobc -fsyntax-only -std=ibm` as-is (inventory §12). They stay in
the golden set; their reference output comes from a compiled copy with the smallest possible change,
made in a temporary directory. `app/` is not touched.

| Program | Problem in the source | Shim |
|---|---|---|
| CBSTM03A | `app/cpy/CUSTREC.cpy` lines 6–22 start with tab characters | Copy of the copybook with tabs expanded (`expand -t 1`) so entries stay within columns 8–72. Expanding to 8 columns pushes the `PIC` clauses past column 72 and still fails. |
| CBEXPORT, CBIMPORT | `RECORD KEY IS EXPORT-SEQUENCE-NUM` names a WORKING-STORAGE field (`CVEXPORT`), not a field in the FD record | The FD record `PIC X(500)` is replaced by a group with a key field at the same offset as `EXPORT-SEQUENCE-NUM` in `CVEXPORT` (offset 27, `PIC 9(9) COMP`), and the RECORD KEY points at it. |

`scripts/migration/cobc-syntax-gate.sh` builds the shims and checks all 14 non-CICS programs. With
it, all 14 pass (output in the PR).

Open points about the shims:

- **Hypothesis:** the shims do not change behavior. Tab expansion only moves text, and the key field
  sits at the same bytes the program writes through `EXPORT-RECORD`. Not shown yet; the CBEXPORT /
  CBIMPORT golden-set ticket has to check it.
- `CBEXPORT.jcl` defines `EXPORT.DATA` with `KEYS(4 28)`, but the copybook puts the field at offset
  27 (inventory §11, **Hypothesis:** a JCL defect). The shim follows the copybook.
- `CBIMPORT` opens a `CARDOUT` file that `CBIMPORT.jcl` has no DD for (inventory §11). The golden-set
  run has to supply one.

If the shims turn out to change behavior, these three programs fall back to the online rule (§4.3):
scenarios written from the source.

## 6. Starting point

- The Java base branch builds: `mvn -B verify` → 6 modules SUCCESS, 41 tests, 0 failures (run again
  for this ticket, see PR).
- Not built there: CORPT00C, CBTRN03C, CBSTM03A/B, CBEXPORT/CBIMPORT, CBTRN01C, a UI, sign-on token
  and role checks, a reader/writer for the fixed-width files. That last one is needed before any
  golden set can be pointed at the Java jobs (prior-work §1).
- Known defects to fix there: double posting on retry, and `TRAN-ID` is random instead of the legacy
  counter (prior-work §1).

## 7. To confirm (open questions, not blockers)

1. **Demo or real deployment?** Is this a demo/reference migration, or does it feed a real
   deployment? It changes how far §4.3 needs to go and whether the security deviations in §3 are
   required.
2. **Timeline.** When is the handover PR to `main` expected?
3. **Effort baseline.** What manual estimate should the Devin effort be compared against? Size of
   the work: 31 programs, about 18,000 non-comment COBOL lines (12,815 online, 5,239 batch and
   utility), 30 copybooks, 17 BMS maps, 12 KSDS. **Hypothesis**, to replace with your own figure:
   at a few hundred lines of COBOL per engineer-day (port and equivalence evidence together), a
   manual team would need several engineer-months.
