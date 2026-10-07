# Pre-migration test strategy: CBACT01C and CBTRN01C

This document describes the executable safety net that pins down the current
behaviour of two CardDemo batch programs before they are ported. Nothing under
`app/` was modified; the programs were compiled and executed as-is with
GnuCOBOL and their real outputs were turned into golden files.

| | |
|---|---|
| Programs | `app/cbl/CBACT01C.cbl` (account extract, JCL `app/jcl/READACCT.jcl`) and `app/cbl/CBTRN01C.cbl` (daily-transaction card/account pre-check) |
| Harness | `test-harness/` – Python 3, standard library only; `test-harness/cobol/` – GnuCOBOL build/run scripts and stubs |
| Goldens | `golden-files/CBACT01C/`, `golden-files/CBTRN01C/`, `golden-files/CBTRN01C/synthetic-rejections/` |
| Checks | `test-harness/RECONCILIATION_CHECKS.md`, `test-harness/reconcile.py` |
| Unit tests | `python3 -m unittest discover -s test-harness/tests -v` (17 tests) |

## 1. Scope

**CBACT01C** reads `ACCTFILE` (VSAM KSDS, key `ACCT-ID`, 300-byte
`CVACT01Y`) sequentially and, per account, writes

* `OUTFILE` – `OUT-ACCT-REC`, fixed 107 bytes; `OUT-ACCT-CURR-CYC-DEBIT` is
  `PIC S9(10)V99 COMP-3`; `OUT-ACCT-REISSUE-DATE` is produced by the assembler
  routine `COBDATFT` (`YYYY-MM-DD` → `YYYYMMDD`); a zero
  `ACCT-CURR-CYC-DEBIT` is replaced by `2525.00`;
* `ARRYFILE` – `ARR-ARRAY-REC`, fixed 110 bytes, `ARR-ACCT-BAL OCCURS 5`
  with constants 1005.00, 1525.00, −1025.00, −2500.00 in occurrences 1–3 and
  `INITIALIZE`d zeros in 4–5;
* `VBRCFILE` – variable records, `VBRC-REC1` (12 bytes) then `VBRC-REC2`
  (39 bytes) per account;
* a DISPLAY of every account field and of both VBRC records.

**CBTRN01C** reads `DALYTRAN` (sequential, 350-byte `CVTRA06Y`), looks each
card up in `XREFFILE` (KSDS, `CVACT03Y`, key `XREF-CARD-NUM`) and the
resulting account in `ACCTFILE`, and DISPLAYs the record, the xref result and
one of: account found, `ACCOUNT <id> NOT FOUND`, or
`CARD NUMBER <n> COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-<id>`. It
opens `CUSTFILE`, `CARDFILE` and `TRANFILE` and never reads or writes them;
its only output is the DISPLAY stream.

Sample data: `app/data/ASCII/*.txt` (fixed width + newline per record;
acctdata 50 × 300, cardxref 50 × 50, carddata 50 × 150, custdata 50 × 500,
dailytran 300 × 350). The EBCDIC originals under `app/data/EBCDIC` were used
only to cross-check the ASCII conversion (§4.6).

## 2. How the golden files were generated

GnuCOBOL 3.1.2 (`cobc`) was available, so **both programs were actually
executed**; no output was derived by hand. One command regenerates everything:

```bash
test-harness/cobol/run_all.sh
```

which runs, in order:

```bash
test-harness/cobol/build.sh                 # cobc -std=ibm -I app/cpy -fsign=EBCDIC -O
test-harness/cobol/load_ksds.sh             # ASCII sample data -> GnuCOBOL indexed files (KSDSLOAD)
test-harness/cobol/run_cbact01c.sh          # -> cobol/work/CBACT01C/{OUTFILE,ARRYFILE,VBRCFILE,display.txt}
test-harness/cobol/run_cbtrn01c.sh          # -> cobol/work/CBTRN01C/display.txt
test-harness/cobol/run_synthetic_rejections.sh   # 3-record CBTRN01C fixture covering both reject paths
test-harness/cobol/run_synthetic_mixed_debit.sh  # 5-account CBACT01C fixture with non-zero ACCT-CURR-CYC-DEBIT
python3 test-harness/generate_goldens.py CBACT01C
python3 test-harness/generate_goldens.py CBTRN01C
python3 test-harness/generate_goldens.py CBTRN01C \
    --work test-harness/cobol/work/synthetic-rejections/CBTRN01C \
    --out  golden-files/CBTRN01C/synthetic-rejections \
    --dailytran test-harness/cobol/work/synthetic-rejections/fixtures/dailytran.txt \
    --cardxref  test-harness/cobol/work/synthetic-rejections/fixtures/cardxref.txt
python3 test-harness/generate_goldens.py CBACT01C \
    --work test-harness/cobol/work/synthetic-mixed-debit/CBACT01C \
    --out  golden-files/CBACT01C/synthetic-mixed-debit \
    --acctdata test-harness/cobol/work/synthetic-mixed-debit/fixtures/acctdata.txt
```

`generate_goldens.py` decodes the real output files with the layouts parsed
from the program source / copybooks, runs the reconciliation suite and writes
`reconciliation.json`. Re-running the sequence is deterministic and reproduces
the committed files byte for byte (`git status` stays clean).

### 2.1 Build details (`test-harness/cobol/`)

* `build.sh` compiles `app/cbl/CBACT01C.cbl` and `app/cbl/CBTRN01C.cbl`
  unmodified with `cobc -x -std=ibm -I app/cpy -fsign=EBCDIC -O`.
  `-fsign=EBCDIC` makes GnuCOBOL read and write the EBCDIC-style zoned
  overpunch (`{ } A–I J–R`) that the ASCII sample data carries, instead of its
  native ASCII convention (`p–y` for negatives).
* `COBDATFT.cbl` is a COBOL stub of `app/asm/COBDATFT.asm`. The assembler was
  read to confirm the behaviour: `CODATECN-TYPE = '2'` (input `YYYY-MM-DD`)
  with `CODATECN-OUTTYPE = '1'` inserts hyphens; with any other out-type
  (CBACT01C passes `'2'`) it copies positions 1–4, 6–7, 9–10 of the input to
  positions 1–8 of the output, giving `YYYYMMDD`; bytes 9–20 of
  `CODATECN-0UT-DATE` are not touched. Any other `CODATECN-TYPE` moves
  `INVALID INPUT` into `CODATECN-ERROR-MSG` and leaves the output date
  untouched. The stub reproduces each of those paths. The in-type `'1'`
  (`YYYYMMDD` → `YYYY-MM-DD`) branch is reproduced too but not exercised.
* `CEE3ABD.cbl` is a stub for the LE abend service: it displays
  `CEE3ABD: USER ABEND U<code>` and stops the run with that return code. It
  is never reached with the sample data (both programs ran with RC 0).
* `KSDSLOAD.cbl` + `load_ksds.sh` load `acctdata`, `cardxref`, `custdata`,
  `carddata` into GnuCOBOL indexed files keyed exactly as the programs'
  `SELECT`s declare (`ACCT-ID`, `XREF-CARD-NUM`, `CUST-ID`, `CARD-NUM`,
  `TRAN-ID`), so keyed `READ`s and the sequential KSDS read behave as on the
  mainframe. `TRANFILE` is created empty – CBTRN01C only opens it.
* `run_cbtrn01c.sh` strips the newline from `dailytran.txt` to produce the
  350-byte fixed sequential `DALYTRAN`.
* `VBRCFILE` is written with `COB_VARSEQ_FORMAT=1`: each record is prefixed
  with a 4-byte big-endian length (`golden-files/CBACT01C/raw/VBRCFILE` is
  50 × (4+12+4+39) = 2950 bytes). This stands in for the z/OS RDW; the
  *record payloads* (12 and 39 bytes) are what the golden JSON captures.

### 2.2 Golden file inventory

| file | records | fields per record |
|------|---------|-------------------|
| `CBACT01C/input-acctdata.json` | 50 | 12 (`CVACT01Y`, filler excluded) |
| `CBACT01C/outfile.json` | 50 | 12 (`OUT-ACCT-REC`) |
| `CBACT01C/arryfile.json` | 50 | 3 top-level: `ARR-ACCT-ID`, `ARR-ACCT-ACTIVE-STATUS`, `ARR-ACCT-BAL` (5 × {`ARR-ACCT-CURR-BAL`, `ARR-ACCT-CURR-CYC-DEBIT`}) |
| `CBACT01C/vbrcfile.json` | 100 (50 REC1 + 50 REC2, alternating) | REC1: `_type`, `_length` + 2; REC2: `_type`, `_length` + 5 |
| `CBACT01C/display.txt` | 50 account blocks | stdout of the run |
| `CBACT01C/raw/{OUTFILE,ARRYFILE,VBRCFILE}` | 5350 / 5500 / 2950 bytes | binary output files as written by the program |
| `CBACT01C/reconciliation.json` | 29 checks, all PASS | |
| `CBACT01C/synthetic-mixed-debit/*` | 5 accounts with `ACCT-CURR-CYC-DEBIT` 10.00 / 0 / 120.50 / −75.25 / 0 | same shapes; 29 checks, all PASS (pins the §7 carry-over) |
| `CBTRN01C/input-dailytran.json` | 300 | 12 (`CVTRA06Y`) |
| `CBTRN01C/input-cardxref.json`, `input-acctdata.json` | 50 / 50 | lookup tables used by the checks |
| `CBTRN01C/outcomes.json` | 300 | 6: `tran_id`, `card_num`, `xref_found`, `acct_id`, `acct_found`, `outcome` |
| `CBTRN01C/display.txt` | 300 transaction blocks | stdout of the run |
| `CBTRN01C/reconciliation.json` | 11 checks, all PASS | |
| `CBTRN01C/synthetic-rejections/*` | 3 transactions: VERIFIED, CARD_NOT_FOUND, ACCOUNT_NOT_FOUND | same shapes; 11 checks, all PASS |

Record identity for comparisons: `ACCT-ID` / `OUT-ACCT-ID` / `ARR-ACCT-ID`
for the account files, position + `VB1-ACCT-ID`/`VB2-ACCT-ID` for VBRCFILE,
`DALYTRAN-ID` / `tran_id` (file order) for the transaction files.

## 3. The harness

* `copybook.py` – parses copybooks and program `01` levels: level numbers,
  `PIC X/9/S9/V` (including `9(n)V9(m)` forms), `COMP`/`COMP-3`/`BINARY`, `OCCURS`, `REDEFINES`,
  `FILLER`, `VALUE` (ignored), `88` levels (skipped). Produces
  `Field(name, offset, length, type, digits, scale, sign, occurs, usage)`
  trees; `flat_layout()` gives the per-byte map.
* `records.py` – decodes/encodes fixed, line-delimited and length-prefixed
  variable record files. Strings keep trailing spaces exactly, numerics become
  decimal strings with the PIC's scale (`"1940.00"`), dates stay as their
  10-character text. Handles zoned decimal with EBCDIC overpunch, ASCII
  (GnuCOBOL) overpunch, unsigned zones, COMP-3 and big-endian COMP, in both
  ASCII and CP037 EBCDIC source bytes. `encode_record(decode_record(x)) == x`
  holds for every sample record (unit-tested).
* `compare.py` – field-by-field comparison of two JSON record sets, matched
  by a key field (or position); nested OCCURS entries are flattened to
  `ARR-ACCT-BAL[1].ARR-ACCT-CURR-BAL`. Every difference is reported as
  `{record_key, field, expected, actual}`; records missing on either side are
  reported with field `<record>`. Default is exact text comparison;
  `--numeric-value` compares numerics as `Decimal` (scale-insensitive) for
  diagnosis only.
* `reconcile.py` – the checks in `RECONCILIATION_CHECKS.md`.
* `generate_goldens.py` – turns a run directory into golden files.

## 4. Parity comparison rules

These are the rules a port's output must satisfy to be declared equivalent.
They are the rules `compare.py` applies by default.

### 4.1 Decimal precision and scale
Every numeric field is compared as a decimal string with **exactly** the
scale of its PIC: `S9(10)V99` → two decimals always (`"0.00"`, `"-1025.00"`,
`"194.00"`), `9(11)` → no decimal point. `"1940"` ≠ `"1940.00"`. Totals in the
reconciliation are exact `Decimal` sums. No floating point anywhere; a port
must use `BigDecimal`/fixed-point and the copybook scale.

### 4.2 Trailing spaces
Alphanumeric (`PIC X`) fields are compared byte-exact at their PIC length,
including trailing spaces (`ACCT-GROUP-ID` is ten spaces,
`OUT-ACCT-REISSUE-DATE` is `"20250520  "`). Text is never trimmed or padded.
`DALYTRAN-ID`, `DALYTRAN-CARD-NUM`, `XREF-CARD-NUM` are `PIC X` in their
copybooks, so their leading zeros are kept verbatim; numeric PICs
(`ACCT-ID PIC 9(11)` → `"1"`) are normalised to a canonical decimal string.

### 4.3 Dates
Dates are text: `YYYY-MM-DD` (10 characters) on input, and whatever the
program moves on output (`YYYYMMDD` + two spaces in `OUT-ACCT-REISSUE-DATE`;
`YYYY` in `VB2-ACCT-REISSUE-YYYY`). No parsing, validation or time-zone logic
is applied anywhere – the harness and the checks treat them as strings, and a
port must reformat by character positions exactly as `COBDATFT` does.

### 4.4 Sign handling
* Input zoned fields use the EBCDIC overpunch convention in the last byte
  (`{`=+0, `A–I`=+1..+9, `}`=−0, `J–R`=−1..−9). GnuCOBOL's native ASCII
  overpunch (`p–y` for negatives) is accepted too.
* Decoded values carry a leading `-` only when non-zero; `}` (negative zero)
  decodes to `"0.00"`, so **negative zero equals positive zero**.
* Zero in a zoned field may be written as `0` (no sign nibble, what `INITIALIZE`
  produces) or `{` (+0, what a `MOVE` produces). Both decode to the same value.
  This is the one place where the raw bytes of a byte-exact port may legitimately
  differ from `golden-files/CBACT01C/raw/ARRYFILE` (bytes 79 and 98 of each
  record, occurrences 4 and 5); the JSON comparison treats them as equal.
* COMP-3: sign nibble `C` positive, `D` negative, `F` unsigned accepted on
  read; written as `C`/`D` (`2525.00` → `0000000252500C`).

### 4.5 Record identity and order
`OUTFILE`/`ARRYFILE`/`VBRCFILE` must be in ascending `ACCT-ID` order (the
KSDS sequential read order), `outcomes.json` in `DALYTRAN` file order.
`compare.py --key` reports missing/extra records as well as field diffs.

### 4.6 EBCDIC vs ASCII
Goldens were produced from the ASCII sample data because that is what the
programs process in this repository's local setup; `records.py` decodes the
EBCDIC originals (`ebcdic=True`, CP037, zone nibbles `C`/`D`/`F`) with the same
layouts. Cross-check result: `dailytran` and `cardxref` decode identically
from both encodings; `acctdata` differs in exactly one field – account
`00000000049` has `ACCT-ADDR-ZIP = "ZEROAPR   "` in EBCDIC and `"A000000000"`
in ASCII. That is a discrepancy in the shipped sample data, not in the
programs, and is recorded here so nobody "fixes" the golden to match the other
encoding. A port fed EBCDIC must be compared against goldens regenerated from
EBCDIC input; the harness supports it, the committed goldens are ASCII-based.

### 4.7 DISPLAY output
`display.txt` is the exact stdout of the run. It is compared textually
(`diff`). The lines are fixed-format COBOL `DISPLAY`s – numeric fields appear
with their PIC width and overpunch sign exactly as the program prints them.

## 5. Reconciliation checks

See `test-harness/RECONCILIATION_CHECKS.md` for the full list (29 checks for
CBACT01C, 11 for CBTRN01C; a check whose evidence is absent – e.g. no
`display.txt` – is emitted as `SKIP`, never silently dropped). Summary of results on the committed goldens:

* CBACT01C: 50 accounts in = 50 OUTFILE = 50 ARRYFILE = 100/2 VBRCFILE.
  Σ `ACCT-CURR-BAL` 12269.00, Σ `ACCT-CREDIT-LIMIT` 233711.00,
  Σ `ACCT-CASH-CREDIT-LIMIT` 122148.00, Σ `ACCT-CURR-CYC-CREDIT` 0.00 – all
  equal in and out. Σ `OUT-ACCT-CURR-CYC-DEBIT` = 126250.00 = 50 × 2525.00
  (every input debit is zero, so the substitution fires on every record).
  Array constants: 50250.00 / 76250.00 / −51250.00 / −125000.00. Every
  output key maps to exactly one input account.
* CBTRN01C: 300 in = 300 outcomes; 300 VERIFIED + 0 + 0.
  Σ `DALYTRAN-AMT` = 104801.54 (positive 129200.83, negative −24399.29).
  301 xref lookups in the display (see §7). Synthetic fixture: 1/1/1.

## 6. How a future Java (or other) port will be judged

A port is accepted for these two programs when, run against the same inputs
(`app/data/ASCII/*` loaded into its own stores the same way `load_ksds.sh`
loads them):

1. **Byte/record parity.** Its `OUTFILE`, `ARRYFILE` and `VBRCFILE` decoded
   with the *same* layouts (`records.py`, or the port's own serializer if it
   emits JSON of the same shape) show **zero mismatches** from
   `golden-files/CBACT01C/{outfile,arryfile,vbrcfile}.json` under
   `compare.py --key OUT-ACCT-ID` / `--key ARR-ACCT-ID` / positional.
   If the port writes the raw files, `cmp` against `raw/` must be clean except
   for the documented zero-sign bytes (§4.4).
2. **Outcome parity.** For CBTRN01C its per-transaction result list equals
   `outcomes.json` field for field (and the synthetic-rejections fixture
   equals its `outcomes.json`), i.e. the same card is rejected/accepted for
   the same reason in the same order.
3. **Reconciliation.** `reconcile.py <job> --golden-dir <port output dir>`
   reports PASS with identical `expected`/`actual` values to the committed
   `reconciliation.json`. This is the check that still works when the port
   changes the file format (e.g. writes JSON or a database table) – the
   business invariants must hold regardless.
4. **Display parity.** `display.txt` is `diff`-clean, or every difference is
   listed and signed off (expected for the §7 items).

Example commands for a port that drops its decoded output into `port-out/`:

```bash
python3 test-harness/compare.py golden-files/CBACT01C/outfile.json  port-out/CBACT01C/outfile.json  --key OUT-ACCT-ID
python3 test-harness/compare.py golden-files/CBACT01C/arryfile.json port-out/CBACT01C/arryfile.json --key ARR-ACCT-ID
python3 test-harness/compare.py golden-files/CBACT01C/vbrcfile.json port-out/CBACT01C/vbrcfile.json
python3 test-harness/compare.py golden-files/CBTRN01C/outcomes.json port-out/CBTRN01C/outcomes.json --key tran_id
python3 test-harness/reconcile.py cbact01c --golden-dir port-out/CBACT01C
python3 test-harness/reconcile.py cbtrn01c --golden-dir port-out/CBTRN01C
diff golden-files/CBACT01C/display.txt port-out/CBACT01C/display.txt
```

If the port's raw files exist, decode them first with the checked-in layouts:

```python
from copybook import parse_file, layout
from records import decode_file
prog = parse_file("app/cbl/CBACT01C.cbl")
recs = decode_file("port-out/OUTFILE", layout(prog, "OUT-ACCT-REC"))
```

## 7. Legacy behaviours a port must know about

These were observed in the real runs and are pinned by the goldens/checks.
Reproduce them, or get an explicit sign-off to diverge and update the goldens.

* **CBACT01C `2525.00` substitution is stateful.** `OUT-ACCT-CURR-CYC-DEBIT`
  is only assigned by `IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO MOVE 2525.00 ...`;
  the program never moves a non-zero input debit into the output and never
  re-initialises `OUT-ACCT-REC` between records. So for a non-zero input the
  output keeps the *previous record's* value, and before the first zero input
  it holds never-assigned WORKING-STORAGE: undefined on z/OS (default
  `NOWSCLEAR`), `LOW-VALUES` under GnuCOBOL, which is not a valid packed
  decimal (the golden records it as `INVALID-COMP-3:00000000000000`).
  `golden-files/CBACT01C/synthetic-mixed-debit` pins this: inputs 10.00 / 0 /
  120.50 / −75.25 / 0 produce undefined / 2525.00 / 2525.00 / 2525.00 /
  2525.00. `CBACT01C-TOTAL-CYC-DEBIT` and `CBACT01C-FIELD-CYC-DEBIT` model
  exactly that (undefined rows excluded; the check detail also shows the
  "business intent" total a port that copies non-zero inputs would produce).
  With the shipped data all inputs are zero, so both readings coincide.
  Decide which semantics the port keeps before feeding non-zero data; if the
  bug is fixed deliberately, change the expected rows in `reconcile.py` and
  regenerate the fixture's `reconciliation.json`.
* **Reissue date padding.** `COBDATFT` writes 8 bytes into a 20-byte field;
  the two bytes that end up in `OUT-ACCT-REISSUE-DATE(9:10)` are whatever was
  in `CODATECN-0UT-DATE(9:2)` – spaces, since WORKING-STORAGE is
  space-initialised here and the `CODATECN-REC` is `INITIALIZE`d per record.
  The golden is `YYYYMMDD␠␠`.
* **ARRYFILE occurrences 4–5** are zero with unsigned zone bytes (§4.4).
* **VBRCFILE** has no RDW on z/OS output datasets as seen by COBOL; the
  4-byte GnuCOBOL length prefix is a container artefact. Compare payloads.
* **CBTRN01C extra lookup after EOF.** `1000-DALYTRAN-GET-NEXT` sets
  `END-OF-DAILY-TRANS-FILE` to `'Y'` but the main loop still performs
  `2000-LOOKUP-XREF` (and `3000-READ-ACCOUNT`) one more time with the previous
  record still in the buffer, so `display.txt` has 301 `SUCCESSFUL READ OF
  XREF` blocks for 300 transactions (and 4 for 3 in the synthetic fixture).
  `outcomes.json` contains 300 rows – the duplicate is *not* a transaction –
  but `CBTRN01C-COUNT-04` pins the display count.
* **CBTRN01C does not write anything.** The outcomes file is a harness
  artefact derived from the DISPLAY stream; a port may expose the same
  information through a log or a return structure as long as it can be
  rendered in the `outcomes.json` shape.
* **Return codes.** Both programs end with RC 0 on the sample data. The
  abend paths (file status ≠ 00 on OPEN/READ/WRITE → `9999-ABEND-PROGRAM` →
  `CEE3ABD` with code 999) are present but not exercised; the stub makes them
  observable if a port's test data triggers them.
* **CBTRN01C `CARD NUMBER ... COULD NOT BE VERIFIED` and `ACCOUNT ... NOT
  FOUND`** are not reachable with the shipped sample data; they are pinned by
  the `synthetic-rejections` golden instead.

## 8. Out of scope

* Everything other than CBACT01C and CBTRN01C (CICS/BMS programs, the other
  batch jobs, the JCL itself, IDCAMS steps).
* `CUSTFILE`, `CARDFILE`, `TRANFILE` content: CBTRN01C opens them only; they
  are loaded so the OPENs succeed but nothing about their content is asserted.
* Performance, memory, job-step return-code handling outside the programs,
  SYSOUT formatting beyond the literal DISPLAY text, and JCL-level behaviours
  (DISP, space allocation, RDW handling by the access method).
* EBCDIC-based goldens (the harness can decode EBCDIC; the committed goldens
  are from the ASCII data, see §4.6).
* Validation of the business meaning of the data (e.g. the `ZEROAPR` zip, the
  identical expiry date on all accounts); the goldens capture what the
  programs do with the data as shipped.
* Error/abend paths (`9999-ABEND-PROGRAM` → `CEE3ABD`, `COBDATFT`
  `INVALID INPUT`) are implemented in the stubs so they are observable, but
  they are not covered by goldens because the sample data cannot trigger them
  without modifying `app/`.
