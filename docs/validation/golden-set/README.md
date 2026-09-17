# Golden-set equivalence harness for the daily posting cycle (`CBTRN02C`)

This directory documents `tests/golden/`, a harness that answers one question
about a converted posting program: **does it produce the same bytes the original
COBOL produces, for inputs whose behaviour we know?**  Nothing here converts
COBOL to anything.  The harness generates synthetic inputs, runs the unmodified
`app/cbl/CBTRN02C.cbl` under GnuCOBOL to obtain the *golden* outputs, and
reconciles any candidate's outputs against them field by field.

## Numbers

The one number to remember is the count of fields reconciled per run with zero
unexplained differences on the reference-vs-reference run.  Everything in this
block is generated from the manifests, reconciliation reports and the copybook
parser by `python3 tests/golden/docs_numbers.py`; `selftest.sh` fails if the
block is stale.

<!-- generated: numbers -->
- **named set, reference vs reference:** 52 records, **610 fields reconciled, 0 differences** (GnuCOBOL primary run vs GnuCOBOL variant run; verdict `EXACT MATCH`, exit 0).
- **volume set, reference vs reference:** 1,945 records, **22,167 fields reconciled, 0 differences** (GnuCOBOL primary run vs GnuCOBOL variant run; verdict `EXACT MATCH`, exit 0).
- **Injected defects:** 9 mutant classes x 2 sets = 18 injected, 18 caught (`tests/golden/selftest.sh`, recorded in `tests/golden/sets/selftest-result.json`).
- **Tolerance path:** 4 of 4 checks passed (a bound that covers the one-cent defect gives `MATCH WITHIN TOLERANCE`, exit 3; a bound that does not still gives `MISMATCH`, exit 1).

| Metric | Value | Derived from |
|---|---|---|
| Named cases | 19 | `sets/named/input/manifest.json` -> `cases` |
| Named-set DALYTRAN records | 22 | `sets/named/input/manifest.json` -> `record_counts` |
| Volume-set DALYTRAN records | 1,200 | `sets/volume/input/manifest.json` -> `record_counts` |
| named set: accepted / rejected (program outcome) | 14 / 8; by reason 0100=1, 0101=1, 0102=3, 0103=3 | `sets/named/expected/prediction-check.json` |
| volume set: accepted / rejected (program outcome) | 1,047 / 153; by reason 0100=49, 0101=33, 0102=52, 0103=19 | `sets/volume/expected/prediction-check.json` |
| named set: generator/program disagreements | 0 of 52 items checked | `sets/named/expected/prediction-check.json` |
| volume set: generator/program disagreements | 0 of 1945 items checked | `sets/volume/expected/prediction-check.json` |
| Fields per record: DALYTRAN (input) | 14 fields, 350 bytes | `app/cpy/CVTRA06Y.cpy` via `layouts.py` |
| Fields per record: XREFFILE (input) | 4 fields, 50 bytes | `app/cpy/CVACT03Y.cpy` via `layouts.py` |
| Fields per record: ACCTFILE (input) | 13 fields, 300 bytes | `app/cpy/CVACT01Y.cpy` via `layouts.py` |
| Fields per record: TCATBALF (input) | 5 fields, 50 bytes | `app/cpy/CVTRA01Y.cpy` via `layouts.py` |
| Fields per record: TRANSACT (output) | 14 fields, 350 bytes | `app/cpy/CVTRA05Y.cpy` via `layouts.py` |
| Fields per record: DALYREJS (output) | 16 fields, 430 bytes | `app/cpy/CVTRA06Y.cpy + app/cbl/CBTRN02C.cbl:180-182` via `layouts.py` |
| Fields per record: ACCTFILE (output) | 13 fields, 300 bytes | `app/cpy/CVACT01Y.cpy` via `layouts.py` |
| Fields per record: TCATBALF (output) | 5 fields, 50 bytes | `app/cpy/CVTRA01Y.cpy` via `layouts.py` |
| named set: output records per file | TRANSACT=14, DALYREJS=8, ACCTFILE=17, TCATBALF=13 | `compare.py` |
| volume set: output records per file | TRANSACT=1,047, DALYREJS=153, ACCTFILE=167, TCATBALF=578 | `compare.py` |
| named set: expected vs exact copy | 52 records, 610 fields, 0 differences, exit 0 (`EXACT MATCH`) | `compare.py` |
| volume set: expected vs exact copy | 1,945 records, 22,167 fields, 0 differences, exit 0 (`EXACT MATCH`) | `compare.py` |
| Mutant classes | 9: `amount_off_by_one_cent`, `reject_moved_to_accepted`, `sign_flipped_on_balance`, `record_dropped`, `two_records_swapped`, `trailing_space_in_text`, `reason_code_changed`, `category_row_dropped`, `return_code_changed` | `mutate.py --list` |
| Reference return code | named=4 / volume=4 | `sets/*/expected/run.json` |
| Candidate (PR #9), named set | verdict `MISMATCH` (exit 1): 52 records, 610 fields reconciled, 0 field differences, 0 control-total mismatches, RETURN-CODE expected `4` got `0` | `candidate-pr9/named/reconciliation.json` |
| Candidate (PR #9), volume set | verdict `MISMATCH` (exit 1): 1,945 records, 22,167 fields reconciled, 0 field differences, 0 control-total mismatches, RETURN-CODE expected `4` got `0` | `candidate-pr9/volume/reconciliation.json` |
<!-- /generated: numbers -->

Candidate wording is copied from `candidate-pr9/*/reconciliation.md`; see
[Candidate result](#candidate-result-pull-request-9).

## What a golden set is

A golden set is one complete, frozen input for the posting cycle plus the
outputs the *original* program produced from it, kept as evidence:

| part | files | produced by |
|---|---|---|
| input | `DALYTRAN`, `XREFFILE`, `ACCTFILE`, `TCATBALF`, `manifest.json` | `tests/golden/generate.py` (seeded, synthetic) |
| expected | `TRANSACT`, `DALYREJS`, post-run `ACCTFILE`, post-run `TCATBALF`, `RETURN-CODE`, `SYSOUT`, `run.json`, `prediction-check.json`, `env.txt` | `tests/golden/run_reference.sh` running unmodified `CBTRN02C` |
| expected-variant | the same outputs from a second GnuCOBOL run with a different `COB_*` configuration | `run_reference.sh --variant` |

```text
 generate.py ──► sets/<set>/input/ ──┬─► CBTRN02C (GnuCOBOL, unmodified) ──► sets/<set>/expected/ ──┐
   seed + cases   DALYTRAN XREFFILE  │      run_reference.sh                  TRANSACT DALYREJS     │
   manifest.json  ACCTFILE TCATBALF  │                                        ACCTFILE TCATBALF     ├─► compare.py ──► reconciliation.{json,md}
                                     └─► candidate implementation ─────────► <candidate>/           │      exit 0 / 2 / 1
                                            same files, same clock              same six outputs    ┘
 mutate.py ──► expected/ + one defect ──► compare.py must exit non-zero and name the field   (selftest.sh)
```

A candidate implementation is fed the *same* input files and its outputs are
compared with `expected/`.  Two sets are committed under `tests/golden/sets/`:
`named` (one record per behaviour we want to pin down) and `volume` (1,000+
records mixing those behaviours so control totals mean something).

All values are synthetic and labelled as such: card numbers start with `0000`
and fail the Luhn check, account / customer / merchant ids start with `9000`,
names are `GOLDEN MERCHANT nnnn`, cities `SYNTHETIC CITY`, descriptions
`GOLDEN CASE <case>` (`manifest.json` -> `synthetic_markers`).

## Posting-cycle inventory (what `CBTRN02C` reads, writes and updates)

Job `app/jcl/POSTTRAN.jcl:23` runs `PGM=CBTRN02C`; its DD statements are at
`app/jcl/POSTTRAN.jcl:28-42`.

| ddname (`ASSIGN`) | layout | organisation / access | opened as | role in the program |
|---|---|---|---|---|
| `DALYTRAN` | `CVTRA06Y` (350) | sequential (`CBTRN02C.cbl:29-32`) | `INPUT` (`:238`) | read once per loop iteration (`:346`); drives the main loop `:202-219` |
| `TRANFILE` -> `TRANSACT` | `CVTRA05Y` (350) | indexed, random, key `FD-TRANS-ID` (`:34-38`) | `OUTPUT` (`:256`) | one `WRITE` per accepted transaction (`:564`) |
| `XREFFILE` | `CVACT03Y` (50) | indexed, random, key `FD-XREF-CARD-NUM` (`:40-44`) | `INPUT` (`:275`) | random `READ` by card number (`:383`) |
| `DALYREJS` | `CVTRA06Y` + 80-byte trailer (430) | sequential (`:46-49`) | `OUTPUT` (`:293`) | one `WRITE` per rejected transaction (`:451`) |
| `ACCTFILE` | `CVACT01Y` (300) | indexed, random, key `FD-ACCT-ID` (`:51-55`) | `I-O` (`:311`) | random `READ` (`:395`) then `REWRITE` of balances (`:554`) |
| `TCATBALF` | `CVTRA01Y` (50) | indexed, random, key `FD-TRAN-CAT-KEY` (`:57-61`) | `I-O` (`:329`) | random `READ` (`:474`); `WRITE` when absent (`:510`) or `REWRITE` (`:528`) |

Record layouts are copied in at `CBTRN02C.cbl:102-126`; the reject trailer is
`WS-VALIDATION-TRAILER` (`:180-182`).  Field tables: [layouts.md](layouts.md).

### Accept / reject decision

`1500-VALIDATE-TRAN` (`CBTRN02C.cbl:370-378`) performs two lookups; the first
non-zero reason stops the chain (`:373-377`).  The main loop posts when
`WS-VALIDATION-FAIL-REASON = 0` and otherwise writes a reject (`:211-216`).

| reason | condition | where |
|---|---|---|
| `100` `INVALID CARD NUMBER FOUND` | `READ XREF-FILE` hits `INVALID KEY` | `:383-387` |
| `101` `ACCOUNT RECORD NOT FOUND` | `READ ACCOUNT-FILE` hits `INVALID KEY` | `:395-399` |
| `102` `OVERLIMIT TRANSACTION` | `ACCT-CREDIT-LIMIT < ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT` | `:403-412` |
| `103` `TRANSACTION RECEIVED AFTER ACCT EXPIRATION` | `ACCT-EXPIRAION-DATE < DALYTRAN-ORIG-TS(1:10)` (string compare, `>=` accepts) | `:414-419` |

Reasons `102` and `103` are evaluated in sequence inside one `READ ... NOT
INVALID KEY` block, so when both hold, `103` is the one written (`:410`, `:417`;
case `overlimit_and_expired`).  Reason `109` at `:556` is set inside the
`REWRITE ... INVALID KEY` handler *after* the transaction has been counted as
posted; no reject record is written for it.  **Inferred - needs owner
confirmation:** that path is unreachable in this harness (the account was just
read by key) and is not represented in any golden set.

### Posting side effects (per accepted transaction, `2000-POST-TRANSACTION`, `:424-443`)

1. `TRAN-*` fields copied from `DALYTRAN-*` (`:425-436`); `TRAN-PROC-TS` from
   `FUNCTION CURRENT-DATE` (`:437-438`, `Z-GET-DB2-FORMAT-TIMESTAMP` `:692-705`).
2. `TCATBALF`: row for (`XREF-ACCT-ID`, `TYPE-CD`, `CAT-CD`) read (`:469-479`);
   if missing a row is created with the amount (`:503-510`), otherwise the amount
   is added (`:527-528`).
3. `ACCTFILE`: `ACCT-CURR-BAL += AMT`; `AMT >= 0` goes to `ACCT-CURR-CYC-CREDIT`,
   `AMT < 0` to `ACCT-CURR-CYC-DEBIT` (`:547-552`), then `REWRITE` (`:554`).
4. `TRANSACT`: `WRITE` (`:564`).

The program ends with `RETURN-CODE 4` when any reject was written (`:229-231`),
after displaying the processed / rejected counts (`:227-228`).

## Environment adaptations (program byte-identical)

`app/cbl/CBTRN02C.cbl` is compiled unchanged (`run_reference.sh` prints its
SHA-256).  Everything below lives in the environment:

| adaptation | why | why it does not change behaviour |
|---|---|---|
| `cobc -x -std=ibm -fsign=EBCDIC -I app/cpy` | `-std=ibm` accepts the IBM dialect and resolves bare `ASSIGN TO ddname` names through the environment; `-fsign=EBCDIC` makes signed zoned fields use the mainframe overpunch (`{A-I` / `}J-R`), which is also how the repository's own ASCII sample data encodes signs | compiler options; no source change.  The sign option fixes the on-disk representation of `S9` fields and is applied identically to golden and candidate expectations |
| `DD_<ddname>=path` (`tests/golden/env/posttran.env`) | GnuCOBOL's equivalent of the `//<ddname> DD` statements in `POSTTRAN.jcl:28-42` | tells the runtime *where* each file is, not how it is read |
| `COB_FILE_PATH` + bare `<ddname>` variables (`env/posttran-variant.env`) | a second, deliberately different file mapping for the reference-vs-reference run | same |
| `COB_CURRENT_DATE="2026-03-15 12:00:00.00"` (value from `manifest.json` -> `frozen_clock`) | `TRAN-PROC-TS` comes from `FUNCTION CURRENT-DATE` (`:693`); without a frozen clock two runs are never byte-identical | the program never compares `CURRENT-DATE` with data; only the stamp value changes.  The `.00` fraction is required (see [findings.md](findings.md)) |
| `tests/golden/cobol/GSIDXUTL.cbl` loader / dumper | GnuCOBOL indexed files are a runtime-specific on-disk format; VSAM KSDS clusters are normally built by IDCAMS outside the program | separate program, run before and after `CBTRN02C`, uses the copybook layouts and the same ddnames; contains no business logic.  It uses `ACCESS MODE IS DYNAMIC` so that fixtures need not be pre-sorted |
| candidate only: `faketime -f "@2026-03-15 12:00:00 x0"` | freezes the JVM clock at the same instant, rate 0 | external to the candidate; equivalent of `COB_CURRENT_DATE` |

`COB_SYNC=true` was tried for the variant run and rejected: GnuCOBOL 3.1.2 with
Berkeley DB segfaults on `OPEN OUTPUT` of an indexed file.  Recorded in
[findings.md](findings.md).

## How to generate, run the reference, compare

All commands run from the repository root and use only `cobc`, `bash`,
`python3` (standard library).

```bash
# 1. generate inputs, compile the unmodified program, build indexed files,
#    run it, dump outputs -> tests/golden/sets/{named,volume}/expected/
bash tests/golden/run_reference.sh --variant     # --variant also writes expected-variant/

# 2. prove the comparator catches defects and that the docs are current
bash tests/golden/selftest.sh

# 3. reconcile any candidate output directory against the golden outputs
python3 tests/golden/compare.py tests/golden/sets/volume/expected <candidate-dir>
#    exit 0 = record files and RETURN-CODE byte-identical, 2 = same records in a
#    different order, 3 = every difference inside a named --tolerance bound,
#    1 = any other field / control-total / return-code difference, a missing or
#    malformed file, or a missing RETURN-CODE.  Writes reconciliation.{json,md}.
#    SYSOUT (the operator log) is reported but informational unless --strict-sysout.

# 4. generate a set by hand (the runner does this for you)
python3 tests/golden/generate.py --set named  --seed 20260315 --out tests/golden/sets/named/input
python3 tests/golden/generate.py --set volume --seed 20260315 --out tests/golden/sets/volume/input

# 5. run the external candidate from pull request #9 (separate checkout; never edited)
bash tests/golden/run_candidate_pr9.sh <candidate-checkout> --set named
bash tests/golden/run_candidate_pr9.sh <candidate-checkout> --set volume
```

Tolerances: none by default.  `compare.py --tolerance FIELD=ABS` (for example
`--tolerance TRAN-AMT=0.01`) is the only way to accept a numeric difference.  A
run that differs only inside the named bounds is reported as `MATCH WITHIN
TOLERANCE` with exit 3 (never 0: the files are not byte-identical), the report
carries a **Tolerance policy in effect** banner naming the field, the bound and
every difference it absorbed, and a control total fed by a tolerated field
(`sum_accepted_amount` from `TRAN-AMT`, `closing_category_balances` from
`TRAN-CAT-BAL`) is absorbed only when every contributing difference was itself
within the bound.  A difference outside the bound stays `MISMATCH`, exit 1.
`selftest.sh` exercises both directions on every set.

### Output of `bash tests/golden/run_reference.sh --variant`

```text
== cobc (GnuCOBOL) 3.1.2.0
== source: app/cbl/CBTRN02C.cbl sha256 708f3cadc555acab63f11e2f3238f5372ac7180e6b01197bf960d96bf0d2e83f (not modified)
== cobc -x -std=ibm -fsign=EBCDIC -I app/cpy -o tests/golden/.work/build/CBTRN02C app/cbl/CBTRN02C.cbl
== cobc -x -std=ibm -fsign=EBCDIC -o tests/golden/.work/build/GSIDXUTL tests/golden/cobol/GSIDXUTL.cbl
generated named set -> tests/golden/sets/named/input
  DALYTRAN=22 XREFFILE=18 ACCTFILE=17 TCATBALF=11
  predicted: accepted=14 rejected=8 by_reason={"0100": 1, "0101": 1, "0102": 3, "0103": 3} (generator prediction, not golden truth)
== named/expected: RETURN-CODE=4  TRANSACTIONS PROCESSED :000000022 TRANSACTIONS REJECTED  :000000008 
   TRANSACT=14 recs  DALYREJS=8 recs  ACCTFILE=17 recs  TCATBALF=13 recs
== named/expected-variant: RETURN-CODE=4  TRANSACTIONS PROCESSED :000000022 TRANSACTIONS REJECTED  :000000008 
   TRANSACT=14 recs  DALYREJS=8 recs  ACCTFILE=17 recs  TCATBALF=13 recs
== prediction check named: 22 records, actual accepted=14 rejected=8 by_reason={"0100": 1, "0101": 1, "0102": 3, "0103": 3}; 0 disagreement(s)
generated volume set -> tests/golden/sets/volume/input
  DALYTRAN=1200 XREFFILE=191 ACCTFILE=167 TCATBALF=411
  predicted: accepted=1047 rejected=153 by_reason={"0100": 49, "0101": 33, "0102": 52, "0103": 19} (generator prediction, not golden truth)
== volume/expected: RETURN-CODE=4  TRANSACTIONS PROCESSED :000001200 TRANSACTIONS REJECTED  :000000153 
   TRANSACT=1047 recs  DALYREJS=153 recs  ACCTFILE=167 recs  TCATBALF=578 recs
== volume/expected-variant: RETURN-CODE=4  TRANSACTIONS PROCESSED :000001200 TRANSACTIONS REJECTED  :000000153 
   TRANSACT=1047 recs  DALYREJS=153 recs  ACCTFILE=167 recs  TCATBALF=578 recs
== prediction check volume: 1200 records, actual accepted=1047 rejected=153 by_reason={"0100": 49, "0101": 33, "0102": 52, "0103": 19}; 0 disagreement(s)
== done
```

### Output of `bash tests/golden/selftest.sh`

```text
  wrote tests/golden/sets/selftest-result.json
docs_numbers.py --check: README.md, layouts.md and findings.md generated blocks match the artefacts
golden-set comparator self-test (compare.py vs mutate.py)
  named    exact-copy                   exit 0 (expected 0)  EXACT MATCH  PASS
  named    amount_off_by_one_cent       exit 1 (expected 1)  named: TRAN-AMT GS03150000000001 sum_accepted_amount  CAUGHT
  named    category_row_dropped         exit 1 (expected 1)  named: missing in candidate 90000000018020003 closing_category_balances  CAUGHT
  named    reason_code_changed          exit 1 (expected 1)  named: WS-VALIDATION-FAIL-REASON GS03150000000002 rejected_by_reason  CAUGHT
  named    record_dropped               exit 1 (expected 1)  named: missing in candidate GS03150000000013 accepted  CAUGHT
  named    reject_moved_to_accepted     exit 1 (expected 1)  named: GS03150000000002 accepted rejected rejected_by_reason  CAUGHT
  named    return_code_changed          exit 1 (expected 1)  named: RETURN-CODE MISMATCH  CAUGHT
  named    sign_flipped_on_balance      exit 1 (expected 1)  named: ACCT-CURR-BAL 90000000001 sign closing_account_balances  CAUGHT
  named    trailing_space_in_text       exit 1 (expected 1)  named: TRAN-MERCHANT-NAME GS03150000000001  CAUGHT
  named    two_records_swapped          exit 2 (expected 2)  named: TRANSACT DIFFERENT ORDER  CAUGHT
  named    tolerance TRAN-AMT=0.01      exit 3 (expected 3)  named: MATCH WITHIN TOLERANCE Tolerance policy in effect WITHIN TOLERANCE ±0.01 TRAN-AMT sum_accepted_amount  PASS
  named    tolerance TRAN-AMT=0.001     exit 1 (expected 1)  named: MISMATCH ±0.001 TRAN-AMT sum_accepted_amount  PASS
  volume   exact-copy                   exit 0 (expected 0)  EXACT MATCH  PASS
  volume   amount_off_by_one_cent       exit 1 (expected 1)  named: TRAN-AMT GS03150000000001 sum_accepted_amount  CAUGHT
  volume   category_row_dropped         exit 1 (expected 1)  named: missing in candidate 90000000166020003 closing_category_balances  CAUGHT
  volume   reason_code_changed          exit 1 (expected 1)  named: WS-VALIDATION-FAIL-REASON GS03150000000003 rejected_by_reason  CAUGHT
  volume   record_dropped               exit 1 (expected 1)  named: missing in candidate GS03150000000603 accepted  CAUGHT
  volume   reject_moved_to_accepted     exit 1 (expected 1)  named: GS03150000000003 accepted rejected rejected_by_reason  CAUGHT
  volume   return_code_changed          exit 1 (expected 1)  named: RETURN-CODE MISMATCH  CAUGHT
  volume   sign_flipped_on_balance      exit 1 (expected 1)  named: ACCT-CURR-BAL 90000000001 sign closing_account_balances  CAUGHT
  volume   trailing_space_in_text       exit 1 (expected 1)  named: TRAN-MERCHANT-NAME GS03150000000001  CAUGHT
  volume   two_records_swapped          exit 2 (expected 2)  named: TRANSACT DIFFERENT ORDER  CAUGHT
  volume   tolerance TRAN-AMT=0.01      exit 3 (expected 3)  named: MATCH WITHIN TOLERANCE Tolerance policy in effect WITHIN TOLERANCE ±0.01 TRAN-AMT sum_accepted_amount  PASS
  volume   tolerance TRAN-AMT=0.001     exit 1 (expected 1)  named: MISMATCH ±0.001 TRAN-AMT sum_accepted_amount  PASS
  docs     docs_numbers.py --check      exit 0 (expected 0)  README/layouts/findings blocks current  PASS
  mutants defined: 9; sets: named volume
  checks passed: 25 of 25  (exact-copy x2 + 9 mutants x2 + 2 tolerance-path x2 + docs sync)
  RESULT: PASS - 18 of 18 injected defects caught; exact copy compares clean; tolerance path 4 of 4
```

### Output of the reference-vs-reference and exact-copy comparisons

```text
$ python3 tests/golden/compare.py tests/golden/sets/named/expected tests/golden/sets/named/expected-variant
compare: tests/golden/sets/named/expected vs tests/golden/sets/named/expected-variant
  TRANSACT  recs expected=14 candidate=14 matched=14 fields=196 diffs=0 missing=0 extra=0 byte-identical=yes
  DALYREJS  recs expected=8 candidate=8 matched=8 fields=128 diffs=0 missing=0 extra=0 byte-identical=yes
  ACCTFILE  recs expected=17 candidate=17 matched=17 fields=221 diffs=0 missing=0 extra=0 byte-identical=yes
  TCATBALF  recs expected=13 candidate=13 matched=13 fields=65 diffs=0 missing=0 extra=0 byte-identical=yes
  RETURN-CODE match
  SYSOUT      match
  52 records, 610 fields reconciled, 0 differences; verdict EXACT MATCH (exit 0)
  reports: <out-dir>/reconciliation.{json,md}
$ python3 tests/golden/compare.py tests/golden/sets/volume/expected tests/golden/sets/volume/expected-variant
compare: tests/golden/sets/volume/expected vs tests/golden/sets/volume/expected-variant
  TRANSACT  recs expected=1047 candidate=1047 matched=1047 fields=14,658 diffs=0 missing=0 extra=0 byte-identical=yes
  DALYREJS  recs expected=153 candidate=153 matched=153 fields=2,448 diffs=0 missing=0 extra=0 byte-identical=yes
  ACCTFILE  recs expected=167 candidate=167 matched=167 fields=2,171 diffs=0 missing=0 extra=0 byte-identical=yes
  TCATBALF  recs expected=578 candidate=578 matched=578 fields=2,890 diffs=0 missing=0 extra=0 byte-identical=yes
  RETURN-CODE match
  SYSOUT      match
  1,945 records, 22,167 fields reconciled, 0 differences; verdict EXACT MATCH (exit 0)
  reports: <out-dir>/reconciliation.{json,md}
$ python3 tests/golden/compare.py tests/golden/sets/named/expected tests/golden/sets/named/expected
compare: tests/golden/sets/named/expected vs tests/golden/sets/named/expected
  TRANSACT  recs expected=14 candidate=14 matched=14 fields=196 diffs=0 missing=0 extra=0 byte-identical=yes
  DALYREJS  recs expected=8 candidate=8 matched=8 fields=128 diffs=0 missing=0 extra=0 byte-identical=yes
  ACCTFILE  recs expected=17 candidate=17 matched=17 fields=221 diffs=0 missing=0 extra=0 byte-identical=yes
  TCATBALF  recs expected=13 candidate=13 matched=13 fields=65 diffs=0 missing=0 extra=0 byte-identical=yes
  RETURN-CODE match
  SYSOUT      match
  52 records, 610 fields reconciled, 0 differences; verdict EXACT MATCH (exit 0)
  reports: <out-dir>/reconciliation.{json,md}
$ python3 tests/golden/compare.py tests/golden/sets/volume/expected tests/golden/sets/volume/expected
compare: tests/golden/sets/volume/expected vs tests/golden/sets/volume/expected
  TRANSACT  recs expected=1047 candidate=1047 matched=1047 fields=14,658 diffs=0 missing=0 extra=0 byte-identical=yes
  DALYREJS  recs expected=153 candidate=153 matched=153 fields=2,448 diffs=0 missing=0 extra=0 byte-identical=yes
  ACCTFILE  recs expected=167 candidate=167 matched=167 fields=2,171 diffs=0 missing=0 extra=0 byte-identical=yes
  TCATBALF  recs expected=578 candidate=578 matched=578 fields=2,890 diffs=0 missing=0 extra=0 byte-identical=yes
  RETURN-CODE match
  SYSOUT      match
  1,945 records, 22,167 fields reconciled, 0 differences; verdict EXACT MATCH (exit 0)
  reports: <out-dir>/reconciliation.{json,md}
```

## Candidate result (pull request #9)

The Spring Batch candidate at head `03912525cf8bf90b8e810b2616f67c99a77d0abd`
(`modernization/posttran-cycle/`) was built with Maven from a separate checkout
and run, unmodified, against both golden inputs by
`tests/golden/run_candidate_pr9.sh`.  The comparator's reports are committed
exactly as produced under [`candidate-pr9/`](candidate-pr9/):

| set | report | console |
|---|---|---|
| named | [`candidate-pr9/named/reconciliation.md`](candidate-pr9/named/reconciliation.md) | [`console.log`](candidate-pr9/named/console.log) |
| volume | [`candidate-pr9/volume/reconciliation.md`](candidate-pr9/volume/reconciliation.md) | [`console.log`](candidate-pr9/volume/console.log) |

Summary lines, verbatim from the comparator:

```text
compare: <repo>/tests/golden/sets/named/expected vs <repo>/tests/golden/sets/named/candidate-pr9
  TRANSACT  recs expected=14 candidate=14 matched=14 fields=196 diffs=0 missing=0 extra=0 byte-identical=yes
  DALYREJS  recs expected=8 candidate=8 matched=8 fields=128 diffs=0 missing=0 extra=0 byte-identical=yes
  ACCTFILE  recs expected=17 candidate=17 matched=17 fields=221 diffs=0 missing=0 extra=0 byte-identical=yes
  TCATBALF  recs expected=13 candidate=13 matched=13 fields=65 diffs=0 missing=0 extra=0 byte-identical=yes
  RETURN-CODE MISMATCH
  SYSOUT      differs (informational; operator log)
  52 records, 610 fields reconciled, 0 differences + RETURN-CODE mismatch; verdict MISMATCH (exit 1)
  reports: <repo>/tests/golden/sets/named/candidate-pr9/reconciliation.{json,md}

compare: <repo>/tests/golden/sets/volume/expected vs <repo>/tests/golden/sets/volume/candidate-pr9
  TRANSACT  recs expected=1047 candidate=1047 matched=1047 fields=14,658 diffs=0 missing=0 extra=0 byte-identical=yes
  DALYREJS  recs expected=153 candidate=153 matched=153 fields=2,448 diffs=0 missing=0 extra=0 byte-identical=yes
  ACCTFILE  recs expected=167 candidate=167 matched=167 fields=2,171 diffs=0 missing=0 extra=0 byte-identical=yes
  TCATBALF  recs expected=578 candidate=578 matched=578 fields=2,890 diffs=0 missing=0 extra=0 byte-identical=yes
  RETURN-CODE MISMATCH
  SYSOUT      differs (informational; operator log)
  1,945 records, 22,167 fields reconciled, 0 differences + RETURN-CODE mismatch; verdict MISMATCH (exit 1)
  reports: <repo>/tests/golden/sets/volume/candidate-pr9/reconciliation.{json,md}
```

What was supplied from outside the candidate to make the comparison possible
(recorded in `candidate-pr9/*/run.json`): file locations through the candidate's own
`posting.*` command-line properties, its `posting.encoding=ASCII` switch (exact
property names are in `console.log`), and the frozen clock via `faketime`.  Building needed a Maven repository mirror in this
environment (`GS_MVN_ARGS="-s <settings.xml>"`); the candidate's own unit tests
were skipped (`-DskipTests`) because this harness is the external check.

## Files in this harness

| path | purpose |
|---|---|
| `tests/golden/layouts.py` | parses `app/cpy/*.cpy` into named fields (offset, length, PIC, sign, scale); no hand-typed offsets |
| `tests/golden/generate.py` | seeded synthetic generator: named cases + volume set + `manifest.json` with the generator's *predictions* |
| `tests/golden/cobol/GSIDXUTL.cbl` | test-only loader/dumper between sequential fixtures and GnuCOBOL indexed files |
| `tests/golden/env/posttran.env`, `posttran-variant.env` | two runtime configurations for the unmodified program |
| `tests/golden/run_reference.sh` | compile, load, run, dump, record toolchain, check predictions |
| `tests/golden/check_prediction.py` | generator prediction vs. program outcome per record (the program wins) |
| `tests/golden/compare.py` | byte / field / control-total / order reconciliation; `reconciliation.{json,md}` |
| `tests/golden/mutate.py`, `selftest.sh` | single-defect mutants, the proof the comparator catches them, and the tolerance-path checks |
| `tests/golden/run_candidate_pr9.sh` | rebuilds the external candidate from a clean `target` (recording its commit and JAR sha256), runs it against the golden inputs and reconciles |
| `tests/golden/docs_numbers.py` | derives every number in this README, `layouts.md` and the disagreement table in `findings.md` from the artefacts; `--check` guards drift |
| `tests/golden/sets/<set>/{input,expected,expected-variant}/` | the committed golden sets |
| `tests/golden/sets/selftest-result.json` | mutants run / caught, written by `selftest.sh`; the source of the "18 injected, 18 caught" figure |
| `docs/validation/golden-set/` | this README, [layouts.md](layouts.md), [findings.md](findings.md), [government-decisions.md](government-decisions.md), [what-this-does-not-prove.md](what-this-does-not-prove.md), `candidate-pr9/` |
