# CardDemo Interest Calculator — CBACT04C on Java 21 / Spring Boot

A strangler-fig migration of the batch COBOL interest calculator
[`app/cbl/CBACT04C.cbl`](../../app/cbl/CBACT04C.cbl) to a self-contained Java 21 / Spring Boot 3.5
/ Spring Batch module.

**The COBOL is still the system of record.** Nothing under `app/`, `asm/`, `proc/`, `csd/`,
`ctl/`, `maclib/` or `scheduler/` is modified, moved or reformatted by this module — it only ever
reads those files, and CI enforces that (`git diff --quiet <base> HEAD -- app/ ...`).

Equivalence is not asserted, it is **measured**: the golden files this module tests against are
the output of the unmodified COBOL program, compiled with GnuCOBOL and executed over the same
datasets. See [Golden-master harness](#golden-master-harness).

---

## Running it

```bash
# build and run the full parity harness
cd modernized/interest-calculator
./mvnw clean verify

# run the job, the equivalent of //STEP15 EXEC PGM=CBACT04C,PARM='2022071800'
./mvnw spring-boot:run -Dspring-boot.run.arguments="\
  runDate=2022071800 \
  inputDirectory=../../app/data/ASCII \
  outputDirectory=target/run"
```

| JCL (`app/jcl/INTCALC.jcl`) | Job parameter |
| --- | --- |
| `PARM='2022071800'` | `runDate` — injected, never hardcoded |
| `//TCATBAL`, `//XREFFILE`, `//ACCTFILE`, `//DISCGRP` DDs | `inputDirectory` (`tcatbal.txt`, `cardxref.txt`, `acctdata.txt`, `discgrp.txt`) |
| `//TRANSACT` DD, and the updated `//ACCTFILE` | `outputDirectory` (`transact.dat`, `acctdata-after.dat`) |

The process return code stands in for the step's RC: `0` when the job completes, `5`
(`BatchStatus.FAILED`) when it fails or the program abends, with no TRANSACT file written.
`ProcessExitCodeTest` asserts this on a real forked JVM, because `BatchStatus` is not what a
scheduler sees.

---

## Traceability: COBOL paragraph → Java method

Every migrated method carries a Javadoc line naming the paragraph it implements.

| CBACT04C paragraph | Java | Notes |
| --- | --- | --- |
| `PROCEDURE DIVISION` mainline | `Cbact04cProgram.run` | control-break loop over TCATBALF |
| `0000-TCATBALF-OPEN` … `0400-TRANFILE-OPEN` | `Cbact04cProgram.run` (file loads) | `TranCatBalanceFile.load`, `CardXrefFile.load`, `DisclosureGroupFile.load`, `AccountFile.load` |
| `1000-TCATBALF-GET-NEXT` | `Cbact04cProgram.getNextTranCatBalanceRecord` | status `'10'` ⇒ `END-OF-FILE` |
| `1050-UPDATE-ACCOUNT` | `Cbact04cProgram.updateAccount` | posts `WS-TOTAL-INT`, zeroes both cycle totals, `REWRITE` |
| `1100-GET-ACCT-DATA` | `Cbact04cProgram.getAccountData` | keyed read on `ACCT-ID` |
| `1110-GET-XREF-DATA` | `Cbact04cProgram.getCardXrefData` | keyed read on the **alternate** key `XREF-ACCT-ID` |
| `1200-GET-INTEREST-RATE` | `Cbact04cProgram.getInterestRate` | `'23'` ⇒ retry with `'DEFAULT'` |
| `1200-A-GET-DEFAULT-INT-RATE` | `Cbact04cProgram.getDefaultInterestRate` | a second miss abends |
| `1300-COMPUTE-INTEREST` | `Cbact04cProgram.computeInterest` | see [Decimal semantics](#decimal-semantics) |
| `1300-B-WRITE-TX` | `Cbact04cProgram.writeTransaction` | builds `TRAN-ID`, writes `CVTRA05Y` |
| `1400-COMPUTE-FEES` | `Cbact04cProgram.computeFees` | **stub in the COBOL, stub here** |
| `Z-GET-DB2-FORMAT-TIMESTAMP` | `Cbact04cProgram.getDb2FormatTimestamp` | `YYYY-MM-DD-HH.MM.SS.MM0000` from an injected `Clock` |
| `9999-ABEND-PROGRAM` / `9910-DISPLAY-IO-STATUS` | `Cbact04cProgram.abend` / `AbendException` | `CEE3ABD` code 999 ⇒ the step fails |

| Copybook | Java type | RECLN |
| --- | --- | --- |
| `CVTRA01Y` `TRAN-CAT-BAL-RECORD` | `TranCatBalanceRecord` | 50 |
| `CVACT03Y` `CARD-XREF-RECORD` | `CardXrefRecord` | 50 |
| `CVTRA02Y` `DIS-GROUP-RECORD` | `DisclosureGroupRecord` | 50 |
| `CVACT01Y` `ACCOUNT-RECORD` | `AccountRecord` | 300 |
| `CVTRA05Y` `TRAN-RECORD` | `TransactionRecord` | 350 |

Each record type documents the byte offset of every field. `PIC 9(11)` account ids are
zero-padded **strings**, not `long`s — leading zeros are part of the key. `PIC S9(n)V99` fields
are `BigDecimal` with scale 2 and occupy `n+2` bytes: in zoned decimal the sign rides as an
overpunch on the last byte and the decimal point is implied, so neither costs a byte.

---

## Decimal semantics

`float` and `double` appear nowhere in this module. The rule the whole migration turns on:

```cobol
COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200
```

* `WS-MONTHLY-INT` is `PIC S9(09)V99` — two decimal places.
* There is **no `ROUNDED` phrase**, so the result is *truncated*, and COBOL truncation is toward
  zero. In Java that is `RoundingMode.DOWN`.
  * It is **not** `HALF_UP`: 100.48 at 15% is 1.256 → COBOL writes **1.25**, `HALF_UP` writes 1.26.
  * It is **not** `FLOOR`: −100.48 at 15% is −1.256 → COBOL writes **−1.25**, `FLOOR` writes −1.26.
    Truncation toward zero and floor only differ on negative amounts, which is exactly the case a
    reviewer should insist on seeing. `DecimalSemanticsTest.negativeBalanceTruncatesTowardZero`
    pins it, and the COBOL oracle agrees.
* There is no `ON SIZE ERROR` phrase either, so high-order digits that do not fit the receiving
  field are dropped silently. `CobolNumeric.store` reproduces both truncations, and
  `ZonedDecimalTest.storeTruncatesHighOrderDigits` covers it.

**Intermediate precision.** The multiplication is exact: two scale-2 operands give a scale-4
product, well inside any COBOL intermediate. The only rounding point is the division, and the
implementation truncates the *exact* quotient to two places
(`product.divide(1200, 2, RoundingMode.DOWN)`). Truncating an already-truncated longer
intermediate to two places gives the same answer as truncating the exact quotient, provided the
intermediate keeps at least two decimals — which IBM Enterprise COBOL's fixed-point intermediate
rules guarantee here. This is an argument, not a proof, which is why it is checked empirically:
the oracle dataset includes 100.00 at 25% (2.08333…, non-terminating), 12 345 678.99 at 25%
(257 201.645625, six decimals) and both signs of 1.256, and the Java output matches the COBOL
byte for byte on all of them.

---

## Golden-master harness

The golden files under `src/test/resources/golden/` were **produced by running the real COBOL**,
not by transcribing what this Java code does.

```
oracle/run-cobol-oracle.sh <dataset-dir> <output-dir> [run-date]
```

1. Compiles the unmodified `app/cbl/CBACT04C.cbl` with GnuCOBOL 3.1.2, copybooks from `app/cpy`,
   using `-fsign=EBCDIC` so signed `DISPLAY` fields use the mainframe trailing overpunch
   (`{`…`I` positive, `}`…`R` negative) that `app/data/ASCII` actually contains.
2. Loads the fixed-width ASCII datasets into GnuCOBOL `INDEXED` files (the KSDS stand-in) with
   `oracle/LOADVSAM.cbl`, honouring the primary and alternate keys of the FDs.
3. Calls `CBACT04C` through `oracle/RUNCB04.cbl`, which passes `PARM-DATE` exactly as
   `app/jcl/INTCALC.jcl` does (`PARM='2022071800'`).
4. Unloads the account master afterwards with `oracle/UNLDACCT.cbl` and splits the RECFM=F
   TRANSACT output into 350-byte lines.

`LOADVSAM`/`RUNCB04`/`UNLDACCT` are new scaffolding in this module; **`CBACT04C.cbl` itself is
compiled verbatim**.

| Dataset | Source | TCATBALF | Result |
| --- | --- | --- | --- |
| `golden/base` | `app/data/ASCII` (TCATBALF 50, DISCGRP 51, ACCTDATA 50, CARDXREF 50) | 50 records | 50 TRANSACT records |
| `golden/edge` | `oracle/datasets/edge`, generated by `oracle/make-edge-dataset.py` | 7 records | 5 TRANSACT records |

Each directory holds `transact.dat`, `acctdata-after.dat` and `cobol-run.log` (the program's
`DISPLAY` output, kept as provenance).

**Why a second dataset?** Every TCATBALF balance in `app/data/ASCII` is `0.00`, so the shipped
sample produces 50 transactions of 0.00 and cannot exercise the arithmetic at all. `edge` uses the
same copybook layouts and the *shipped* `discgrp.txt` rates, and adds the cases that matter:
truncation at the third decimal, a negative balance, a zero balance, a zero rate, a direct
disclosure-group hit, a `DEFAULT` fallback and an eight-figure balance.

### What the tests compare

* `CobolOracleParityTest` — every `CVTRA05Y` field except the two timestamps, record by record,
  for both datasets, plus the whole account master after the run, byte for byte.
* `InterestCalculationJobIntegrationTest` — the same thing through the real Spring Batch job.
* `DecimalSemanticsTest` — the named edge cases, including a table showing `HALF_UP` disagreeing
  with the COBOL on three real records.

`TRAN-ORIG-TS` and `TRAN-PROC-TS` are the **only** nondeterministic outputs (`FUNCTION
CURRENT-DATE`). The oracle's timestamps are its own wall-clock time, so the harness excludes those
52 bytes from the record comparison, asserts they match the DB2 26-character shape on the COBOL
side, and asserts the Java side reproduces the injected `Clock` exactly.

### What invalidates the golden files

Re-run `oracle/run-cobol-oracle.sh` and `oracle/verify-golden.py` (CI does both on every PR) if
any of these change:

* `app/cbl/CBACT04C.cbl`, or any of the five copybooks it uses;
* `app/data/ASCII/{tcatbal,cardxref,acctdata,discgrp}.txt`;
* `oracle/make-edge-dataset.py` or the generated `oracle/datasets/edge`;
* the run date, which is baked into all 16 characters of every `TRAN-ID`;
* the GnuCOBOL version or its `-fsign` setting.

---

## Behaviours intentionally NOT migrated

| Behaviour | Why |
| --- | --- |
| `1400-COMPUTE-FEES` | The COBOL body is a comment (`To be implemented`) plus `EXIT`. Kept as an empty, clearly marked stub. Inventing fee logic would be a silent behaviour change. |
| CICS/VSAM plumbing (`OPEN`/`CLOSE` status ladders, `CEE3ABD`) | Collapsed into file loads and `AbendException`. The observable effect — the step fails loudly with no partial TRANSACT file — is preserved; the `APPL-RESULT`/`APPL-AOK` bookkeeping is not user-visible. |
| `DISPLAY` to SYSOUT | Captured into `InterestCalculationResult.console` instead of being printed, so tests can assert on it. The message texts are preserved verbatim. |
| Physical VSAM KSDS (CI/CD of IDCAMS `DEFINE CLUSTER`) | Datasets are read as fixed-width files and indexed in memory; the batch is small (tens of thousands of records) and this keeps the module dependency-free. Ordering semantics of a KSDS sequential read *are* reproduced (records are sorted by key on load). |
| `//TRANSACT` as RECFM=F with no delimiters | The Java writer emits one 350-byte record per line. Strip the newlines to get the byte-identical RECFM=F image; the golden comparison is done record by record, so nothing is hidden by this. |
| The `TRAN-DESC` tail and the 20-byte `TRAN-RECORD` FILLER | See residual risks — these bytes are never assigned by the program. |

### Preserved legacy defects (deliberately not "fixed")

* **The last account group is never rewritten.** The mainline's `ELSE PERFORM 1050-UPDATE-ACCOUNT`
  branch is unreachable: `PERFORM UNTIL END-OF-FILE = 'Y'` re-tests before each iteration, so the
  loop exits before the `ELSE` can run. The final account therefore keeps its old balance and its
  un-zeroed cycle totals. The Java keeps the same (unreachable) branch and the same outcome;
  `DecimalSemanticsTest.lastAccountGroupIsNeverRewritten` documents it and the COBOL oracle
  confirms it. **This is a real bug in the legacy program** and a candidate for a follow-up change
  — but a migration is the wrong place to change behaviour.
* **A zero balance still writes a 0.00 transaction.** The write is gated on the *rate*
  (`IF DIS-INT-RATE NOT = 0`), not the balance, so a zero-balance category produces a zero-amount
  TRANSACT record. All 50 records from the shipped dataset are of exactly this kind.

---

## Residual risks — what a human must check

1. **GnuCOBOL is not z/OS.** The oracle is GnuCOBOL 3.1.2 on Linux with `-fsign=EBCDIC`. It agrees
   with the datasets' sign encoding and with the copybook layouts, but IBM Enterprise COBOL is the
   real target. Re-running the harness on z/OS (or z/OS Connect / AWS Blu Age) and diffing against
   `golden/` is the single highest-value verification left.
2. **Uninitialised WORKING-STORAGE.** `1300-B-WRITE-TX` fills `TRAN-DESC` with `STRING`, which
   leaves bytes 25–100 untouched, and never assigns the 20-byte `FILLER` at the end of
   `TRAN-RECORD`. Those bytes therefore hold whatever WORKING-STORAGE was initialised to: spaces
   under GnuCOBOL, which is what the Java writes. On z/OS this is formally undefined and may be
   `X'00'`. Check a real z/OS TRANSACT dump before treating those bytes as significant downstream.
3. **Intermediate-precision argument.** The division rounding argument above is reasoned from the
   language rules and confirmed on six oracle records, not proven for all inputs. If production
   balances can reach the `S9(09)V99` limits, re-run the oracle over production-scale values.
4. **`WS-TRANID-SUFFIX` is `PIC 9(06)`.** Beyond 999 999 transactions in one run the counter wraps
   and `TRAN-ID`s repeat. The COBOL does the same (no `ON SIZE ERROR`), and the Java reproduces the
   wrap — but at production volumes this is a live collision risk worth raising.
5. **Alternate key uniqueness.** `XREF-ACCT-ID` is declared without `WITH DUPLICATES`, so the Java
   loader rejects a dataset with two cards on one account. Confirm production XREF really is unique
   on account id; if it is not, the COBOL would silently read the first matching record.
6. **Date fields are carried as text.** `ACCT-OPEN-DATE` and friends are `PIC X(10)` and are never
   interpreted by CBACT04C, so they are round-tripped verbatim rather than parsed into
   `LocalDate`. Any downstream consumer that expects validation will not get it here.
7. **Timestamp granularity.** `Z-GET-DB2-FORMAT-TIMESTAMP` keeps hundredths of a second and pads
   `'0000'`; two transactions written in the same centisecond get identical timestamps. Preserved
   as-is.

---

## Layout

```
modernized/interest-calculator/
├── pom.xml, mvnw, mvnw.cmd, .mvn/           Maven wrapper (script-only, no committed jar)
├── oracle/
│   ├── run-cobol-oracle.sh                  compile + run the real CBACT04C
│   ├── LOADVSAM.cbl / RUNCB04.cbl / UNLDACCT.cbl   new scaffolding around it
│   ├── make-edge-dataset.py                 generates oracle/datasets/edge
│   └── verify-golden.py                     re-checks the committed goldens against a fresh run
└── src/
    ├── main/java/com/carddemo/interestcalc/
    │   ├── copybook/   CVTRA01Y, CVTRA02Y, CVTRA05Y, CVACT01Y, CVACT03Y + zoned-decimal codec
    │   ├── file/       KSDS stand-ins and COBOL file statuses
    │   ├── program/    Cbact04cProgram — the migration itself
    │   └── batch/      Spring Batch job and tasklet
    └── test/…          the parity harness and the golden files
```
