# CardDemo COBOL → Python Migration — Five-Phase Prompt Pack

**Repository:** `COG-GTM/aws-mainframe-modernization-carddemo` (ref: `main`)
**Migration target:** Move COBOL/CICS/VSAM business logic into idiomatic Python.
**First-migration target:** `app/cbl/CBACT04C.cbl` — the batch interest calculator.

This document contains five **self-contained, copy-pasteable prompts** intended to be handed,
one at a time, to an autonomous engineering agent (Devin-style). Each prompt assumes the agent
has **no prior context** and is written in second-person imperative. Every prompt is grounded in
concrete facts extracted by reading the actual source in this repo (the COBOL program, its
copybooks, the `INTCALC` JCL, and the sample data files).

> **How to use:** Run the prompts in order (Phase 1 → 5). Each later phase reads the artifacts
> produced by the earlier phase rather than re-deriving them. Each prompt below is delimited by a
> heading and a fenced block so a presenter can copy any single prompt verbatim.

---

## Ground-truth reference (read before running any prompt)

These facts were confirmed by reading the source and are embedded into the prompts below. They are
collected here so the reader can sanity-check the prompts; the prompts themselves repeat what each
phase needs.

### `CBACT04C` file-control (from `app/cbl/CBACT04C.cbl`, ENVIRONMENT DIVISION)

| Logical file | ASSIGN name | Organization | Access | Record key | Copybook | Open mode |
|---|---|---|---|---|---|---|
| `TCATBAL-FILE` | `TCATBALF` | INDEXED | SEQUENTIAL | `FD-TRAN-CAT-KEY` | `CVTRA01Y` | `INPUT` |
| `XREF-FILE` | `XREFFILE` | INDEXED | RANDOM | `FD-XREF-CARD-NUM` (alt: `FD-XREF-ACCT-ID`) | `CVACT03Y` | `INPUT` |
| `ACCOUNT-FILE` | `ACCTFILE` | INDEXED | RANDOM | `FD-ACCT-ID` | `CVACT01Y` | `I-O` |
| `DISCGRP-FILE` | `DISCGRP` | INDEXED | RANDOM | `FD-DISCGRP-KEY` | `CVTRA02Y` | `INPUT` |
| `TRANSACT-FILE` | `TRANSACT` | SEQUENTIAL | SEQUENTIAL | (none) | `CVTRA05Y` | `OUTPUT` |

Note that `ACCOUNT-FILE` is opened `I-O` (read **and** rewrite), not read-only — the program
mutates account balances.

### Copybook record layouts (from `app/cpy/`)

**`CVTRA01Y` — `TRAN-CAT-BAL-RECORD` (RECLN 50):**
```
05 TRAN-CAT-KEY.
   10 TRANCAT-ACCT-ID   PIC 9(11).     *> 11 bytes, zoned unsigned
   10 TRANCAT-TYPE-CD   PIC X(02).     *> 2
   10 TRANCAT-CD        PIC 9(04).     *> 4
05 TRAN-CAT-BAL         PIC S9(09)V99. *> 11 bytes display signed, 2 implied decimals
05 FILLER               PIC X(22).     *> 22  => total 50
```

**`CVACT01Y` — `ACCOUNT-RECORD` (RECLN 300):**
```
05 ACCT-ID                PIC 9(11).     *> 11
05 ACCT-ACTIVE-STATUS     PIC X(01).     *> 1
05 ACCT-CURR-BAL          PIC S9(10)V99. *> 12
05 ACCT-CREDIT-LIMIT      PIC S9(10)V99. *> 12
05 ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99. *> 12
05 ACCT-OPEN-DATE         PIC X(10).     *> 10
05 ACCT-EXPIRAION-DATE    PIC X(10).     *> 10 (sic: misspelled in source)
05 ACCT-REISSUE-DATE      PIC X(10).     *> 10
05 ACCT-CURR-CYC-CREDIT   PIC S9(10)V99. *> 12
05 ACCT-CURR-CYC-DEBIT    PIC S9(10)V99. *> 12
05 ACCT-ADDR-ZIP          PIC X(10).     *> 10
05 ACCT-GROUP-ID          PIC X(10).     *> 10
05 FILLER                 PIC X(178).    *> 178 => total 300
```

**`CVACT03Y` — `CARD-XREF-RECORD` (RECLN 50):**
```
05 XREF-CARD-NUM PIC X(16). *> 16
05 XREF-CUST-ID  PIC 9(09). *> 9
05 XREF-ACCT-ID  PIC 9(11). *> 11
05 FILLER        PIC X(14). *> 14 => total 50
```

**`CVTRA02Y` — `DIS-GROUP-RECORD` (RECLN 50):**
```
05 DIS-GROUP-KEY.
   10 DIS-ACCT-GROUP-ID PIC X(10).     *> 10
   10 DIS-TRAN-TYPE-CD  PIC X(02).     *> 2
   10 DIS-TRAN-CAT-CD   PIC 9(04).     *> 4
05 DIS-INT-RATE         PIC S9(04)V99. *> 6 bytes display signed, 2 implied decimals
05 FILLER               PIC X(28).     *> 28 => total 50
```

**`CVTRA05Y` — `TRAN-RECORD` (RECLN 350):**
```
05 TRAN-ID            PIC X(16).     *> 16
05 TRAN-TYPE-CD       PIC X(02).     *> 2
05 TRAN-CAT-CD        PIC 9(04).     *> 4
05 TRAN-SOURCE        PIC X(10).     *> 10
05 TRAN-DESC          PIC X(100).    *> 100
05 TRAN-AMT           PIC S9(09)V99. *> 11
05 TRAN-MERCHANT-ID   PIC 9(09).     *> 9
05 TRAN-MERCHANT-NAME PIC X(50).     *> 50
05 TRAN-MERCHANT-CITY PIC X(50).     *> 50
05 TRAN-MERCHANT-ZIP  PIC X(10).     *> 10
05 TRAN-CARD-NUM      PIC X(16).     *> 16
05 TRAN-ORIG-TS       PIC X(26).     *> 26  (nondeterministic: wall clock)
05 TRAN-PROC-TS       PIC X(26).     *> 26  (nondeterministic: wall clock)
05 FILLER             PIC X(20).     *> 20 => total 350
```

> **PIC interpretation note for the agent:** these copybooks declare numeric fields with `PIC 9`
> and `PIC S9...V99` and the FDs read records `INTO` working-storage with `READ ... INTO`. The sample
> data files in `app/data/` are human-readable fixed-width text (see below), i.e. the numeric fields
> are **zoned/display** (one byte per digit), not `COMP-3` packed. The agent MUST confirm this by
> inspecting the bytes (Phase 1) rather than assuming packing. `V99` is an *implied* decimal point —
> there is no literal `.` in the stored field; the rightmost 2 digit positions are the fractional part.

### Core business logic (from `CBACT04C` PROCEDURE DIVISION)

- **Driver loop** (`PROCEDURE DIVISION USING EXTERNAL-PARMS`): sequentially reads
  `TCATBAL-FILE`. On each record it detects an **account break** (`TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM`).
  On a break (and not the first record) it calls `1050-UPDATE-ACCOUNT` for the prior account, resets
  `WS-TOTAL-INT` to 0, then loads the new account (`1100-GET-ACCT-DATA`) and its xref
  (`1110-GET-XREF-DATA`). At EOF it performs a final `1050-UPDATE-ACCOUNT` for the last account.
- **Rate lookup** (`1200-GET-INTEREST-RATE`): builds `FD-DISCGRP-KEY` from `ACCT-GROUP-ID` +
  `TRANCAT-TYPE-CD` + `TRANCAT-CD` and reads `DISCGRP-FILE`. If the read returns file status `'23'`
  (record-not-found / INVALID KEY), it substitutes `'DEFAULT'` into `FD-DIS-ACCT-GROUP-ID` and
  re-reads via `1200-A-GET-DEFAULT-INT-RATE`.
- **Interest computation** (`1300-COMPUTE-INTEREST`):
  ```cobol
  COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
  ADD WS-MONTHLY-INT TO WS-TOTAL-INT
  ```
  `WS-MONTHLY-INT` is `PIC S9(09)V99`. The `COMPUTE` has **no `ROUNDED` phrase**, so the result is
  **truncated** (toward zero) to 2 decimal places. Interest is only computed when `DIS-INT-RATE NOT = 0`.
- **Transaction write** (`1300-B-WRITE-TX`): increments a global counter `WS-TRANID-SUFFIX`
  (`PIC 9(06)`, never reset), builds `TRAN-ID` by `STRING PARM-DATE, WS-TRANID-SUFFIX DELIMITED BY SIZE`
  (10-char parm date concatenated with the 6-digit zero-padded suffix = 16 chars), sets
  `TRAN-TYPE-CD = '01'`, `TRAN-CAT-CD = '05'`, `TRAN-SOURCE = 'System'`,
  `TRAN-DESC = 'Int. for a/c ' || ACCT-ID`, `TRAN-AMT = WS-MONTHLY-INT`,
  `TRAN-MERCHANT-ID = 0`, merchant name/city/zip = spaces, `TRAN-CARD-NUM = XREF-CARD-NUM`, and both
  `TRAN-ORIG-TS` and `TRAN-PROC-TS` to the current DB2-format timestamp (`Z-GET-DB2-FORMAT-TIMESTAMP`).
- **Account update** (`1050-UPDATE-ACCOUNT`): `ADD WS-TOTAL-INT TO ACCT-CURR-BAL`, set
  `ACCT-CURR-CYC-CREDIT = 0` and `ACCT-CURR-CYC-DEBIT = 0`, then `REWRITE` the account record.
- **`1400-COMPUTE-FEES`**: body is `EXIT` only with comment `* To be implemented` — a **stub**.
- **Timestamp source** (`Z-GET-DB2-FORMAT-TIMESTAMP`): `MOVE FUNCTION CURRENT-DATE TO COBOL-TS`,
  then reformats into `DB2-FORMAT-TS` `PIC X(26)` as
  `YYYY-MM-DD-HH.MM.SS.<hundredths>0000` (hundredths is the 2-digit `COB-MIL`; trailing literal `0000`).
- **Abend path** (`9999-ABEND-PROGRAM`): `CALL 'CEE3ABD' USING ABCODE, TIMING` with `ABCODE = 999`.
  This is a Language Environment (LE) abend — it must be stubbed when running the COBOL as an oracle.
- **Program parameter** (`LINKAGE SECTION` `EXTERNAL-PARMS`): `PARM-LENGTH PIC S9(04) COMP`,
  `PARM-DATE PIC X(10)`. The `INTCALC` JCL passes `PARM='2022071800'` (a 10-char run date).

### `INTCALC` JCL (from `app/jcl/INTCALC`)

`STEP15 EXEC PGM=CBACT04C,PARM='2022071800'`. DD mapping:
`TCATBALF→TCATBALF.VSAM.KSDS`, `XREFFILE→CARDXREF.VSAM.KSDS` (+`XREFFIL1→CARDXREF.VSAM.AIX.PATH`),
`ACCTFILE→ACCTDATA.VSAM.KSDS`, `DISCGRP→DISCGRP.VSAM.KSDS`, `TRANSACT→SYSTRAN(+1)` GDG, new
sequential output `RECFM=F,LRECL=350`.

### Sample data (from `app/data/`)

`app/data/EBCDIC/*.PS` and `app/data/ASCII/*.txt` hold fixed-length flat records that are the source
for emulating the indexed VSAM files. Confirmed sizes (file size ÷ record length = record count):
`AWS.M2.CARDDEMO.TCATBALF.PS` = 2500 B / 50 = 50 records;
`AWS.M2.CARDDEMO.DISCGRP.PS` = 2550 B / 50 = 51 records;
`AWS.M2.CARDDEMO.ACCTDATA.PS` = 15000 B / 300 = 50 records;
`AWS.M2.CARDDEMO.CARDXREF.PS` = 2500 B / 50 = 50 records. ASCII equivalents:
`tcatbal.txt`, `discgrp.txt`, `acctdata.txt`, `cardxref.txt`.

---

# Prompt 1 — SCAFFOLD / UNDERSTAND

```text
You are an autonomous software engineer. You have NO prior context. Your job in this phase is to
explore and dissect a legacy mainframe application end-to-end and produce written scaffolding
artifacts that later migration phases will consume. You will NOT write any production Python in
this phase — only analysis documents.

REPOSITORY
- Clone/open the repo COG-GTM/aws-mainframe-modernization-carddemo at ref `main`.
- This is "CardDemo", a COBOL/CICS/VSAM credit-card management application used as an AWS
  mainframe-modernization baseline. It has an ONLINE (CICS) subsystem and a BATCH (JCL) subsystem
  that share VSAM datasets.
- Directory layout under app/: cbl/ (COBOL programs), cpy/ (copybooks), bms/ (3270 screen maps),
  cpy-bms/ (generated map copybooks), jcl/ (batch jobs), proc/ (JCL procs), csd/ (CICS resource
  defs), ctl/, catlg/, maclib/, asm/ (Assembler), data/ (with EBCDIC/ and ASCII/ sample datasets),
  scheduler/ (Control-M). Optional add-on modules: app-authorization-ims-db2-mq/,
  app-transaction-type-db2/, app-vsam-mq/. Root also has diagrams/, samples/, scripts/, README.md.

TASKS — produce the following artifacts as markdown under a new docs folder `migration/phase1/`:

(a) SYSTEM MAP — enumerate:
    - Online CICS transaction programs (the CO* programs: signon COSGN00C, menu COMEN01C,
      account COACTVWC/COACTUPC, card COCRDLIC/COCRDSLC/COCRDUPC, transaction
      COTRN00C/COTRN01C/COTRN02C, reports CORPT00C, bill pay COBIL00C, admin COADM01C,
      user mgmt COUSR00C-COUSR03C). Confirm each by reading app/cbl/ — do not assume.
    - Batch COBOL programs (CBACT01C-CBACT03C readers, CBACT04C interest calculator,
      CBTRN01C-CBTRN03C daily transaction validate/post/report, CBCUS01C customer reader,
      CBSTM03A/CBSTM03B statement generation, CBEXPORT/CBIMPORT branch migration, COBSWAIT wait).
    - JCL job flow in app/jcl/: setup/refresh jobs (ACCTFILE, CARDFILE, CUSTFILE, XREFFILE,
      TRANFILE, DISCGRP, TRANCATG, TRANTYPE, TCATBALF, OPENFIL, CLOSEFIL, DEFGDGB/DEFGDGD,
      ESDSRRDS, TRANIDX) and processing jobs (POSTTRAN→CBTRN02C, INTCALC→CBACT04C,
      COMBTRAN SORT, CREASTMT→CBSTM03A, TRANREPT→CBTRN03C, TRANBKP). Map each job to the
      program it runs and the datasets it reads/writes.
    - Shared VSAM datasets and their copybooks (from README + app/data/EBCDIC/): ACCTDATA
      (CVACT01Y), CARDDATA (CVACT02Y), CUSTDATA (CVCUS01Y), CARDXREF (CVACT03Y), DALYTRAN
      (CVTRA06Y), TRANSACT.VSAM.KSDS (CVTRA05Y), DISCGRP (CVTRA02Y), TRANCATG (CVTRA04Y),
      TRANTYPE (CVTRA03Y), TCATBALF (CVTRA01Y), USRSEC (CSUSR01Y).
    - Optional modules and the Assembler utilities in app/asm/ (COBDATFT date-format conversion,
      MVSWAIT interval timer) and the date utility CSUTLDTC.cbl.

(b) DATA CONTRACTS — for EVERY copybook in app/cpy/ that is referenced by the batch interest path
    and its neighbors, extract a precise field table: field name, PIC clause, byte offset (0-based),
    byte length, signed/unsigned, zoned/packed(COMP-3)/binary(COMP)/display, number of implied
    decimal places (from V), and total record length. At minimum cover: CVTRA01Y
    (TRAN-CAT-BAL-RECORD, 50), CVACT01Y (ACCOUNT-RECORD, 300), CVACT03Y (CARD-XREF-RECORD, 50),
    CVTRA02Y (DIS-GROUP-RECORD, 50), CVTRA05Y (TRAN-RECORD, 350). Also catalog COCOM01Y
    (CARDDEMO-COMMAREA online routing/state, including the 88-level user-type flags admin 'A' /
    user 'U'), CVTRA03Y/CVTRA04Y/CVTRA06Y/CVTRA07Y, and CSUSR01Y. CRITICAL: determine empirically
    whether numeric fields are stored zoned/display or COMP-3 packed by hex-dumping the matching
    flat files in app/data/EBCDIC/ and app/data/ASCII/ — state your evidence; do not assume.

(c) BUSINESS RULES AS WRITTEN — open app/cbl/CBACT04C.cbl IN FULL (entire PROCEDURE DIVISION) and
    transcribe the actual logic, paragraph by paragraph. You MUST capture, verbatim from the source:
    - the driver loop and the account-break logic (TRANCAT-ACCT-ID vs WS-LAST-ACCT-NUM, WS-FIRST-TIME);
    - the interest formula in 1300-COMPUTE-INTEREST:
      `COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200` — note there is NO ROUNDED
      phrase (COBOL truncates toward zero to the receiving field's 2 decimal places, S9(09)V99);
    - the rate-lookup + DEFAULT fallback on DISCGRP file status '23' (1200-GET-INTEREST-RATE /
      1200-A-GET-DEFAULT-INT-RATE);
    - the TRAN-ID construction `STRING PARM-DATE, WS-TRANID-SUFFIX` (10-char run date + 6-digit
      zero-padded global counter, never reset per account) and the fixed field assignments in
      1300-B-WRITE-TX (TRAN-TYPE-CD '01', TRAN-CAT-CD '05', TRAN-SOURCE 'System',
      TRAN-DESC 'Int. for a/c ' + ACCT-ID, TRAN-AMT = WS-MONTHLY-INT, merchant fields, card num);
    - the account update in 1050-UPDATE-ACCOUNT (ADD WS-TOTAL-INT TO ACCT-CURR-BAL, zero the two
      cycle fields, REWRITE).
    Also transcribe (briefly) the posting logic in app/cbl/CBTRN02C.cbl and the validation rules in
    app/cbl/CBTRN01C.cbl so later phases have context for the next migration targets.

(d) DEAD / STUBBED CODE INVENTORY — list paragraphs/sections that are stubs or unreachable. You MUST
    include 1400-COMPUTE-FEES in CBACT04C (its body is `EXIT` with the comment "To be implemented").
    Search the other batch programs for similar stubs and note them.

(e) NONDETERMINISM AUDIT — this is the most important artifact. Enumerate EVERY output field that is
    NOT a pure function of the input data, and for each one name the exact COBOL paragraph/line where
    the value originates. For CBACT04C specifically you MUST identify:
    - TRAN-ORIG-TS and TRAN-PROC-TS (PIC X(26)) — both set from `FUNCTION CURRENT-DATE` via
      Z-GET-DB2-FORMAT-TIMESTAMP; format YYYY-MM-DD-HH.MM.SS.<hundredths>0000. These depend on the
      wall clock and are nondeterministic.
    - Classify TRAN-ID as DETERMINISTIC given the PARM-DATE input and the sequential WS-TRANID-SUFFIX
      counter (document that the suffix is a global monotonically increasing sequence, not clock-based).
    - Flag any other clock/date/sequence/random sources you find anywhere in the program.

(f) ORACLE-FEASIBILITY ASSESSMENT — decide and justify whether the original COBOL can be executed as
    a golden-master oracle. Specifically evaluate: can CBACT04C be compiled and run with (i) the LE
    abend call `CALL 'CEE3ABD'` stubbed/replaced, and (ii) the four INDEXED VSAM input files emulated
    from the fixed-length flat `.PS` sample files in app/data/EBCDIC/ (or the ASCII equivalents in
    app/data/ASCII/) — e.g. via GnuCOBOL with line-sequential/indexed file emulation, or a z/OS-like
    runtime? If running the COBOL is feasible, describe the exact setup. If NOT feasible in this
    environment, state that expected output records must be HAND-DERIVED from the documented arithmetic
    and say so explicitly. Record the decision; Phase 4 depends on it.

VERIFICATION INPUTS — describe precisely, by inspecting app/data/EBCDIC/ and app/data/ASCII/, the
sample files that feed the interest calc: TCATBALF (CVTRA01Y, 50-byte records), CARDXREF (CVACT03Y,
50-byte), ACCTDATA (CVACT01Y, 300-byte), DISCGRP (CVTRA02Y, 50-byte). Report record counts and a few
decoded example records for each.

OUTPUT — write all of the above as markdown files under migration/phase1/ (e.g. system-map.md,
data-contracts.md, business-rules.md, dead-code.md, nondeterminism-audit.md, oracle-feasibility.md).
Do not fabricate field names, programs, or arithmetic: if you cannot confirm something by reading the
file, write "UNCONFIRMED" and explain what is missing. Do not modify any COBOL source.
```

---

# Prompt 2 — PLAN + JIRA

```text
You are an autonomous software engineer. You have NO prior context beyond the Phase 1 scaffolding
described below. Do NOT re-derive Phase 1 analysis — READ it and build on it.

PRECONDITION
- The repo COG-GTM/aws-mainframe-modernization-carddemo (ref `main`) contains a folder
  migration/phase1/ with these artifacts produced by a prior phase: system-map.md,
  data-contracts.md, business-rules.md, dead-code.md, nondeterminism-audit.md, oracle-feasibility.md.
  Read all of them first. If any are missing, STOP and report which are missing rather than guessing.

TASK 1 — PHASED MIGRATION PLAN
Produce a phased plan (write it to migration/phase2/migration-plan.md) to migrate the CardDemo
batch business logic from COBOL into idiomatic Python, one program at a time. For each phase define:
explicit ENTRY criteria, explicit EXIT criteria, and inter-phase DEPENDENCIES. The phases should
mirror this five-phase loop (Understand → Plan → Build → Verify → Automate) and explicitly sequence
the programs to migrate after the first one.

TASK 2 — CONFIRM THE FIRST-MIGRATION TARGET
Confirm or revise the first-migration target. The recommendation is app/cbl/CBACT04C.cbl (the batch
interest calculator), for these reasons — restate and validate them against the Phase 1 docs:
  - self-contained batch program (no CICS/screen/commarea dependencies);
  - deterministic-ish arithmetic (a single COMPUTE; only two output timestamp fields are
    nondeterministic, per the Phase 1 nondeterminism audit);
  - clear, finite file-I/O contracts (4 indexed inputs + 1 sequential output, copybooks
    CVTRA01Y/CVACT03Y/CVACT01Y/CVTRA02Y/CVTRA05Y);
  - excellent golden-master candidate because sample data exists in app/data/.
If the Phase 1 oracle-feasibility assessment changes this calculus, revise the recommendation and
justify it.

TASK 3 — TARGET PYTHON ARCHITECTURE
Define the target Python architecture for a CLEAN, NEW target repo (not inside the COBOL tree). Write
it to migration/phase2/target-architecture.md. It MUST specify:
  - Module structure (e.g. a package `carddemo/` with submodules for records, files, programs, clock,
    cli) and where the migrated CBACT04C entrypoint lives.
  - A COPYBOOK-DRIVEN RECORD PARSER layer: a declarative description of each copybook's fields
    (name, offset, length, type, decimals) that drives encode/decode of fixed-width records, so new
    programs reuse the same parser. Cover at least CVTRA01Y, CVACT01Y, CVACT03Y, CVTRA02Y, CVTRA05Y.
  - FIXED-POINT / DECIMAL STRATEGY: use Python `decimal.Decimal` to mirror COBOL `S9(n)V99` semantics.
    Specify that the interest COMPUTE has no ROUNDED phrase, so results must be TRUNCATED toward zero
    to 2 decimal places (Decimal quantize with ROUND_DOWN), and that field storage must re-truncate to
    the declared scale. Specify how signed zoned/display values are read/written to match the bytes in
    app/data/.
  - FILE-EMULATION LAYER: how RANDOM (keyed) reads against ACCTFILE/XREFFILE/DISCGRP and the
    SEQUENTIAL read of TCATBALF are emulated from the flat fixed-length sample files in
    app/data/EBCDIC/ (or app/data/ASCII/), including the alternate-key read of XREF by ACCT-ID and the
    DISCGRP not-found→'DEFAULT' fallback (file status '23').
  - CLOCK-INJECTION ABSTRACTION: an injectable clock interface so the two nondeterministic timestamp
    fields (TRAN-ORIG-TS / TRAN-PROC-TS) are produced from an injected time source rather than the
    real system clock, making output reproducible. Specify the DB2 timestamp format
    YYYY-MM-DD-HH.MM.SS.<hundredths>0000.

TASK 4 — JIRA STRUCTURE
Structure the plan as JIRA-style work items. Per this org's convention, create a SINGLE Epic for the
overall migration with stories/sub-tasks underneath it (do not scatter many top-level tickets on the
board). Use the US-Federal (UF) project. Write the proposed Epic + stories/sub-tasks (summaries +
descriptions + entry/exit criteria + dependencies) to migration/phase2/jira-plan.md FIRST and present
it for review. Only create the actual JIRA items after the plan is approved. Suggested breakdown:
Epic "CardDemo COBOL→Python migration"; stories for (1) record-parser + decimal layer,
(2) file-emulation layer, (3) clock injection, (4) migrate CBACT04C, (5) golden-master harness,
(6) playbook/skills automation.

OUTPUT — markdown under migration/phase2/. Ground every reference in real file paths and the Phase 1
artifacts. Do not invent programs, fields, or arithmetic.
```

---

# Prompt 3 — BUILD / EXECUTE

```text
You are an autonomous software engineer. You have NO prior context beyond the Phase 1 and Phase 2
artifacts described below. This is the EXECUTION prompt: you will write production Python.

PRECONDITION
- Read migration/phase1/ (especially business-rules.md, data-contracts.md, nondeterminism-audit.md)
  and migration/phase2/ (target-architecture.md) in the repo
  COG-GTM/aws-mainframe-modernization-carddemo (ref `main`). If missing, STOP and report.
- Also OPEN AND READ the source of truth directly: app/cbl/CBACT04C.cbl (entire PROCEDURE DIVISION)
  and copybooks app/cpy/CVTRA01Y.cpy, CVACT01Y.cpy, CVACT03Y.cpy, CVTRA02Y.cpy, CVTRA05Y.cpy. The
  Python must match the COBOL, not a paraphrase of it.

TASK — Migrate app/cbl/CBACT04C.cbl (the batch interest calculator) into idiomatic Python in the
clean target repo defined by the Phase 2 architecture. The program reads transaction-category
balances, joins account + disclosure-group data to derive an interest rate, computes monthly
interest, writes interest transaction records, and updates account balances.

FAITHFULLY REPRODUCE THE FOLLOWING (exact behavior from the COBOL):

1. INPUTS / FILE CONTRACTS
   - TCATBAL-FILE (copybook CVTRA01Y, TRAN-CAT-BAL-RECORD, 50 bytes): read SEQUENTIALLY in key order.
     Key = TRAN-CAT-KEY = TRANCAT-ACCT-ID PIC 9(11) + TRANCAT-TYPE-CD PIC X(02) + TRANCAT-CD PIC 9(04).
     Value field used: TRAN-CAT-BAL PIC S9(09)V99.
   - XREF-FILE (CVACT03Y, CARD-XREF-RECORD, 50 bytes): RANDOM read by alternate key ACCT-ID
     (XREF-ACCT-ID PIC 9(11)); used to obtain XREF-CARD-NUM PIC X(16) for the output transaction.
   - ACCOUNT-FILE (CVACT01Y, ACCOUNT-RECORD, 300 bytes): RANDOM read by ACCT-ID PIC 9(11); opened I-O
     (you will read and later rewrite it). Fields used: ACCT-GROUP-ID PIC X(10), ACCT-CURR-BAL
     PIC S9(10)V99, ACCT-CURR-CYC-CREDIT / ACCT-CURR-CYC-DEBIT PIC S9(10)V99.
   - DISCGRP-FILE (CVTRA02Y, DIS-GROUP-RECORD, 50 bytes): RANDOM read by key
     DIS-ACCT-GROUP-ID PIC X(10) + DIS-TRAN-TYPE-CD PIC X(02) + DIS-TRAN-CAT-CD PIC 9(04). Field used:
     DIS-INT-RATE PIC S9(04)V99. If the keyed read returns "record not found" (COBOL file status '23'
     / INVALID KEY), substitute the literal 'DEFAULT' into the group-id portion of the key and re-read;
     this is the DEFAULT disclosure-group fallback.
   - TRANSACT-FILE (CVTRA05Y, TRAN-RECORD, 350 bytes): SEQUENTIAL OUTPUT; interest transactions are
     appended here in write order.

2. CONTROL FLOW (driver loop, account break)
   - Iterate TCATBAL records in order. Maintain WS-LAST-ACCT-NUM and a first-time flag.
   - On a change of TRANCAT-ACCT-ID (account break): if not the first account, perform UPDATE-ACCOUNT
     for the PREVIOUS account; then reset the running total interest to 0, load the new ACCOUNT record
     and the new XREF record (by ACCT-ID).
   - For every TCATBAL record: build the DISCGRP key from ACCT-GROUP-ID + TRANCAT-TYPE-CD + TRANCAT-CD,
     look up the rate (with DEFAULT fallback). Only if DIS-INT-RATE != 0 do you compute interest and
     write a transaction (COMPUTE-FEES is a no-op stub — replicate it as a no-op).
   - At end of input, perform a FINAL UPDATE-ACCOUNT for the last account.

3. INTEREST ARITHMETIC (exact)
   - monthly_interest = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
   - The COBOL receiving field WS-MONTHLY-INT is PIC S9(09)V99 and the COMPUTE has NO ROUNDED phrase,
     so the result is TRUNCATED toward zero to 2 decimal places. Implement with decimal.Decimal and an
     explicit quantize to 2 places using ROUND_DOWN (truncate toward zero). Do NOT use float.
   - Accumulate monthly_interest into a running total (WS-TOTAL-INT, S9(09)V99) per account.

4. OUTPUT TRANSACTION FIELDS (1300-B-WRITE-TX) — set exactly:
   - TRAN-ID PIC X(16) = PARM-DATE (10 chars, the run-date program parameter) concatenated with a
     6-digit zero-padded global sequence counter (WS-TRANID-SUFFIX, PIC 9(06)) that increments on every
     transaction written and is NEVER reset between accounts. (INTCALC JCL passes PARM='2022071800'.)
   - TRAN-TYPE-CD = '01'; TRAN-CAT-CD = '05' (PIC 9(04) => stored "0005"); TRAN-SOURCE = 'System';
     TRAN-DESC PIC X(100) = 'Int. for a/c ' followed by ACCT-ID; TRAN-AMT PIC S9(09)V99 = monthly_interest;
     TRAN-MERCHANT-ID = 0; TRAN-MERCHANT-NAME / CITY / ZIP = spaces; TRAN-CARD-NUM = XREF-CARD-NUM.
   - TRAN-ORIG-TS and TRAN-PROC-TS PIC X(26) = the DB2-format timestamp from the INJECTED CLOCK
     (see below), both set to the same value.

5. ACCOUNT UPDATE (1050-UPDATE-ACCOUNT) — for each completed account: add the running total interest
   to ACCT-CURR-BAL, set ACCT-CURR-CYC-CREDIT = 0 and ACCT-CURR-CYC-DEBIT = 0, then write the updated
   account record back (mirror the COBOL REWRITE). Persist account changes through the file-emulation
   layer.

6. CLOCK INJECTION (do NOT call the real clock directly)
   - The COBOL uses FUNCTION CURRENT-DATE to build a DB2 timestamp formatted
     YYYY-MM-DD-HH.MM.SS.<hundredths>0000 (PIC X(26); the milliseconds position holds 2 digits of
     hundredths followed by literal '0000'). Obtain the time from an injectable clock abstraction so a
     fixed clock yields reproducible output. The run date for TRAN-ID comes from the PARM-DATE program
     argument, NOT from the clock.

DELIVERABLES
- Idiomatic, typed Python implementing the above in the Phase 2 module layout, including the
  copybook-driven record parser, the Decimal-based fixed-point handling, the flat-file emulation of the
  indexed reads (including the XREF alternate-key read and the DISCGRP DEFAULT fallback), and the
  clock abstraction. Provide a CLI entrypoint that mirrors INTCALC (takes the run-date parameter and the
  input/output file paths). Use the app/data/ sample files as default inputs for a smoke run.
- Include an exact field-mapping table (COBOL field -> Python attribute -> offset/length/decimals) and a
  short note documenting the decimal precision/truncation rules in the code or a README.
- If, while reading the source, you find any behavior that contradicts this prompt, follow the SOURCE
  and document the discrepancy rather than silently diverging. Do not fabricate fields or arithmetic.
```

---

# Prompt 4 — VERIFY / PROVE

```text
You are an autonomous software engineer. You have NO prior context beyond the artifacts below. Your
job is to PROVE the Python migration of CBACT04C is behavior-equivalent to the COBOL, using a
production-grade differential / golden-master test harness.

PRECONDITION
- The Python migration of app/cbl/CBACT04C.cbl exists (Phase 3) in the target repo. Read the Phase 1
  artifacts in migration/phase1/ — ESPECIALLY oracle-feasibility.md and nondeterminism-audit.md — and
  the Phase 2 target-architecture.md. If the oracle decision or the nondeterminism audit is missing,
  STOP and report.

ORACLE (choose per the Phase 1 oracle-feasibility decision)
- PATH A — RUN THE ORIGINAL COBOL as the oracle: compile and execute app/cbl/CBACT04C.cbl (e.g. with
  GnuCOBOL) against the sample data, with:
    * the LE abend CALL 'CEE3ABD' stubbed/replaced (so error paths don't kill the process uncontrollably),
    * the four INDEXED VSAM inputs EMULATED from the fixed-length flat sample files in
      app/data/EBCDIC/ (or app/data/ASCII/): TCATBALF (CVTRA01Y, 50B), CARDXREF (CVACT03Y, 50B; needs
      the ACCT-ID alternate key), ACCTDATA (CVACT01Y, 300B), DISCGRP (CVTRA02Y, 50B),
    * the program parameter PARM-DATE set to '2022071800' (matching app/jcl/INTCALC).
  Capture the resulting TRANSACT output (CVTRA05Y, 350B records) and the rewritten ACCTDATA as the
  golden master.
- PATH B — HAND-DERIVED EXPECTED RECORDS: if Phase 1 concluded the COBOL cannot be run in this
  environment, construct the expected TRANSACT records and expected account updates by hand from the
  documented arithmetic (interest = truncate_toward_zero((TRAN-CAT-BAL * DIS-INT-RATE)/1200, 2 dp);
  account-balance update; TRAN-ID = PARM-DATE + 6-digit sequence). Document each expected record's
  derivation so the golden master is auditable.
State which path you took and why, citing the Phase 1 decision.

NONDETERMINISM NORMALIZATION (mandatory before diffing)
- Per the Phase 1 nondeterminism audit, the fields TRAN-ORIG-TS and TRAN-PROC-TS (PIC X(26)) are
  wall-clock dependent. Normalize them before comparison: either run the Python side with a FIXED
  injected clock and the COBOL oracle under a pinned clock/TZ, or mask/blank both timestamp fields in
  BOTH outputs prior to diffing. Confirm TRAN-ID is treated as DETERMINISTIC (PARM-DATE + sequential
  suffix) and is NOT masked. Document exactly which byte ranges are normalized.

DIFFING + PASS/FAIL
- Implement RECORD-LEVEL diffing (every output transaction record, in order) and FIELD-LEVEL diffing
  (decode each 350-byte record via the copybook layout and compare field by field). Also diff the
  rewritten ACCOUNT records (300-byte) field by field, focusing on ACCT-CURR-BAL,
  ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT.
- For packed/zoned/signed decimal fields (S9(n)V99): compare by DECODED numeric value (Decimal), not
  raw bytes, AND additionally assert the raw encoded bytes match after normalization, so both the
  numeric value and the on-disk representation are verified. Document how the sign and implied decimal
  are handled in the comparison.
- Define explicit PASS/FAIL criteria: PASS = record counts equal AND every non-normalized field equal
  across all records (numeric fields equal as Decimals and byte-equal after normalization). FAIL =
  any count mismatch, missing/extra record, or any field mismatch; emit a precise per-record,
  per-field diff report identifying the first and all divergences.

DELIVERABLES
- A runnable harness (e.g. `pytest` + a CLI) that produces expected (oracle) output, runs the Python
  CBACT04C with a fixed clock, normalizes, diffs, and reports PASS/FAIL with a human-readable diff.
- A short report (migration/phase4/verification-report.md) recording the oracle path used, the
  normalized fields/byte-ranges, the pass/fail result, and any discrepancies found. Do not weaken the
  comparison to force a pass; if outputs differ, report the divergence and its likely COBOL-vs-Python
  cause.
```

---

# Prompt 5 — LEARNING LOOP / AUTOMATION

```text
You are an autonomous software engineer. You have NO prior context beyond the artifacts below. Your
job is to turn the single-program migration of CBACT04C into a REPEATABLE, ACCELERATING migration loop
so each subsequent COBOL program is migrated faster and more reliably.

PRECONDITION
- Phases 1-4 are complete for app/cbl/CBACT04C.cbl in the repo
  COG-GTM/aws-mainframe-modernization-carddemo (ref `main`): migration/phase1/ analysis,
  migration/phase2/ plan + target architecture, a working Python migration, and a passing
  golden-master harness (migration/phase4/verification-report.md). Read them. If missing, STOP and
  report.

TASK 1 — PLAYBOOK
Create or update migration/playbook.md capturing the end-to-end, proven procedure used for CBACT04C,
generalized into ordered, checklist-style steps any agent can follow for the NEXT program:
Understand → Plan → Build → Verify → Automate, with the entry/exit gates from Phase 2. Reference the
real artifacts and commands that worked.

TASK 2 — SKILLS (reusable, extracted from Phases 1-4)
Create a skills/ directory (e.g. migration/skills/) with proven, reusable building blocks. Each skill
must be concrete enough to drop into the next migration:
  - COPYBOOK PARSER skill: given a copybook (PIC clauses), produce a Python field map with
    name/offset/length/decimals and encode/decode of fixed-width records. Must already handle the
    layouts proven in Phase 3: CVTRA01Y (50), CVACT01Y (300), CVACT03Y (50), CVTRA02Y (50),
    CVTRA05Y (350), including group keys.
  - DECIMAL / PACKED-ZONED skill: read/write signed zoned-display (and COMP-3 if encountered) numeric
    fields, and apply COBOL arithmetic semantics — specifically TRUNCATION TOWARD ZERO when a COMPUTE
    has no ROUNDED phrase (as in CBACT04C's interest COMPUTE), with quantize to the field's declared
    scale (e.g. V99 => 2 dp). Include the rule "re-truncate on store to the receiving field's PIC".
  - CLOCK-INJECTION / NONDETERMINISM-NORMALIZATION skill: an injectable clock producing the DB2
    timestamp format YYYY-MM-DD-HH.MM.SS.<hundredths>0000, plus a normalization helper that masks
    known nondeterministic fields (e.g. TRAN-ORIG-TS/TRAN-PROC-TS) before golden-master diffing, while
    leaving deterministic fields (e.g. TRAN-ID) intact.
  - GOLDEN-MASTER HARNESS skill: parameterized record/field diffing for fixed-width records given a
    copybook field map, with Decimal-aware numeric comparison and byte-equality-after-normalization.
Each skill file should follow the repo's skill format (front-matter name + description) so it is
discoverable, and reference where it was first proven (CBACT04C).

TASK 3 — NEXT-PROGRAM SELECTION + FASTER ONBOARDING
Define how the next program is chosen and onboarded using the skills above. Recommend a concrete next
target from the batch suite (e.g. CBTRN02C posting or CBACT01C-03C readers) with rationale based on
shared copybooks/data already covered (e.g. reuse of CVACT01Y/CVACT03Y parsers). Give a step-by-step
"onboard the next program in N steps" recipe that reuses the parser/decimal/clock/harness skills
instead of rebuilding them.

TASK 4 — ACCELERATION METRICS
Define metrics to track that the loop is accelerating, and where to record them (e.g.
migration/metrics.md): per-program lead time (start→passing harness), % lines/fields covered by reused
skills vs. net-new code, number of new copybooks parsed vs. reused, defects caught by the golden-master
before merge, and clock/nondeterminism issues found. Specify a simple format to log these per program.

OUTPUT — migration/playbook.md, migration/skills/*, and migration/metrics.md. Ground everything in the
real artifacts and proven code from Phases 1-4. Do not fabricate; if a pattern was not actually proven
in Phases 1-4, mark it as PROPOSED rather than PROVEN.
```

---

## Appendix — confirmation status of embedded facts

All copybook layouts, the file-control table, the interest formula, the TRAN-ID construction, the
account-update logic, the `1400-COMPUTE-FEES` stub, the timestamp format, the LE abend call, the
`PARM='2022071800'` value, and the sample-file record counts above were **confirmed by reading the
source files** in this repo (`app/cbl/CBACT04C.cbl`, `app/cpy/CVTRA0{1,2,5}Y.cpy`,
`app/cpy/CVACT0{1,3}Y.cpy`, `app/jcl/INTCALC`, and `app/data/EBCDIC/*.PS`).

The following were intentionally **NOT** fully confirmed and are flagged for the agent to verify in
Phase 1 rather than assumed here:
- **Zoned vs. packed (COMP-3) storage of numeric fields.** The sample `.PS`/`.txt` files appear to be
  fixed-width human-readable text (consistent with zoned/display, one byte per digit), but the agent
  must hex-dump the bytes to confirm encoding and sign representation before relying on it.
- **Exact COBOL intermediate-precision rules** for `(TRAN-CAT-BAL * DIS-INT-RATE) / 1200`. The
  receiving field is `S9(09)V99` and there is no `ROUNDED`, so the documented behavior is truncation to
  2 dp toward zero; the agent should validate intermediate-result handling against the chosen oracle
  (or compiler) in Phase 4 rather than assuming a specific intermediate scale.
- **Layouts of copybooks not directly on the interest path** (e.g. `CVTRA03Y`, `CVTRA04Y`, `CVTRA06Y`,
  `CVTRA07Y`, `COCOM01Y`, `CSUSR01Y`) — referenced for the system map but to be extracted in Phase 1.
