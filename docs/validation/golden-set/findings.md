# Findings

Two kinds of entry are recorded here.  **Generator/program disagreements** are
records where `manifest.json`'s prediction and the program's actual outcome
differ (`tests/golden/check_prediction.py` runs after every reference run and
writes `expected/prediction-check.json`; the program wins).  **Behaviours worth
knowing** are things the program does that the generator had to be taught,
each with the citation that explains it, and **environment findings** are
GnuCOBOL behaviours that had to be worked around without touching the program.

Anything not backed by a `path:line` is marked **Inferred - needs owner
confirmation**.

## 1. Generator/program disagreements (current sets)

| set | records | items checked | disagreements |
|---|---:|---:|---:|
| named | 22 | 52 | 0 |
| volume | 1,200 | 1,945 | 0 |

Source: `tests/golden/sets/named/expected/prediction-check.json` and
`tests/golden/sets/volume/expected/prediction-check.json` (`"disagreements": []`).
Items checked = one per `DALYTRAN` record (accept/reject + reason), one per
closing account balance, one per closing category balance.

One disagreement class did occur while the harness was being built and was a
checker bug, not a program behaviour: closing balances were compared as text
(`-428.8` vs `-428.80`) and reported as differing.  `check_prediction.py` now
decodes both sides to `Decimal` before comparing.  No disagreement about
accept/reject, reason code, or a balance *value* has been observed.

## 2. Behaviours the generator encodes because the program does them

Each of these is a place where a reader's first expectation (and a converted
program written from a specification rather than from the source) could differ
from what `CBTRN02C` does.  Each has at least one named case so the golden set
pins it down.

### 2.1 A negative amount makes `ACCT-CURR-CYC-DEBIT` *more negative*, which then *raises* the temporary balance

`2800-UPDATE-ACCOUNT-REC` adds the (negative) amount to `ACCT-CURR-CYC-DEBIT`
(`app/cbl/CBTRN02C.cbl:551`); it does not subtract its magnitude.  The
over-limit test computes `CYC-CREDIT - CYC-DEBIT + AMT` (`:403-405`), so a
debit balance that the program itself has driven negative *adds* to the
temporary balance on the next transaction.  Cases: `negative_amount_credit`,
`debit_sign_interaction`, `credit_relieves_overlimit`.  **Inferred - needs
owner confirmation:** whether `ACCT-CURR-CYC-DEBIT` is meant to hold a
negative running total.

### 2.2 Zero is a credit

`IF DALYTRAN-AMT >= 0` (`:548`) routes a zero amount to `ACCT-CURR-CYC-CREDIT`.
The category row is still created or rewritten with `+0.00` added (`:507`,
`:527`).  Case: `zero_amount`.

### 2.3 When both over-limit and expiry conditions hold, reason `103` is written

The two `IF`s run in sequence inside one `NOT INVALID KEY` block (`:407-419`);
the second `MOVE 103` (`:417`) overwrites the `MOVE 102` (`:410`).  A
first-failing-check implementation would report `102`.  Case:
`overlimit_and_expired`.

### 2.4 The expiry check is a 10-character string comparison, and equality accepts

`ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)` (`:414`) compares
`YYYY-MM-DD` text with the first ten characters of the `YYYY-MM-DD HH:MM:SS...`
timestamp.  An account expiring *on* the transaction date is accepted; one
expiring the day before is rejected.  Cases: `expiry_boundary_equal`,
`expiry_boundary_day_before`.

### 2.5 A missing category row is created, not rejected

`READ TCATBAL-FILE ... INVALID KEY` sets `WS-CREATE-TRANCAT-REC = 'Y'`
(`:474-478`) and a `DISPLAY` goes to SYSOUT (`:476-477`); status `23` is
accepted as success (`:481`); `2700-A-CREATE-TCATBAL-REC` initialises a row
and adds the amount (`:503-510`).  Case: `category_not_in_tcatbal`; in the
volume set 167 rows are created that way (411 seeded, 578 closing; 167
`Creating.` lines in `expected/SYSOUT`).

### 2.6 Transaction validation stops at the first non-zero reason only between lookups

`1500-VALIDATE-TRAN` skips the account lookup when the card lookup failed
(`:372-376`), so a record can never carry both `100` and another reason; but
inside the account lookup the later check overrides the earlier one (2.3).

### 2.7 Reason `109` never produces a reject record

`MOVE 109` at `:556-558` sits inside `REWRITE ... INVALID KEY` in
`2800-UPDATE-ACCOUNT-REC`, which runs *after* the record was routed to
`2000-POST-TRANSACTION` (`:212`).  The `2500-WRITE-REJECT-REC` path is not
reached and `WS-REJECT-COUNT` is not incremented.  Not exercised by any set.
**Inferred - needs owner confirmation:** on VSAM this path would require the
account record to disappear between `READ` and `REWRITE`.

### 2.8 `RETURN-CODE` is `4` whenever a reject was written

`:229-231`.  The named and volume sets both contain rejects, so the golden
`RETURN-CODE` is `4` in both.  This is what the external candidate does not
reproduce (see `candidate-pr9/*/reconciliation.md`, `RETURN-CODE` row).

### 2.9 Sums that would overflow are not in the golden set

`S9(09)V99` holds at most `999,999,999.99` (`app/cpy/CVTRA06Y.cpy:10`); the
account balance and limit fields are `S9(10)V99` (`app/cpy/CVACT01Y.cpy:7-9`)
so a single maximum transaction fits.  Cases `max_amount` and
`max_negative_amount` post `+999,999,999.99` and `-999,999,999.99`.  The
program has no `ON SIZE ERROR` on `ADD DALYTRAN-AMT TO TRAN-CAT-BAL` (`:527`;
`TRAN-CAT-BAL` is `S9(09)V99`, `app/cpy/CVTRA01Y.cpy:9`), so a category
balance *can* overflow.
**Inferred - needs owner confirmation:** truncation on overflow is
compiler-defined and would differ between Enterprise COBOL and GnuCOBOL; the
generator deliberately keeps category totals below the limit.

## 3. Environment findings (GnuCOBOL 3.1.2, program unchanged)

### 3.1 `COB_CURRENT_DATE` must carry an explicit fraction

With `COB_CURRENT_DATE="2026-03-15 12:00:00"` GnuCOBOL 3.1.2 froze the seconds
but kept the wall-clock hundredths, so `TRAN-PROC-TS` (`:437-438`, built at
`:692-705`) differed between two runs.  `"2026-03-15 12:00:00.00"` freezes the
whole value.  The value is written once by the generator into
`manifest.json` -> `frozen_clock` and read from there by both env files.

### 3.2 `COB_SYNC=true` crashes indexed `OPEN OUTPUT`

Intended as one of the variant settings; GnuCOBOL 3.1.2 with the Berkeley DB
indexed backend terminates with `SIGSEGV` before writing a record.  Removed
from `tests/golden/env/posttran-variant.env`; the remaining variant settings
are listed in that file.

### 3.3 Sequential-access indexed loads need sorted input

The first loader declared `ACCESS MODE IS SEQUENTIAL` and received file status
`21` (key out of sequence) because generated fixtures are in case order, not
key order.  `GSIDXUTL.cbl` uses `ACCESS MODE IS DYNAMIC` for the indexed side,
so fixture order is irrelevant.  Dumps are always in key order.

### 3.4 Sign overpunch

GnuCOBOL's default ASCII overpunch for negative zoned digits is `p`..`y`;
the repository's sample data and the mainframe use `}`,`J`..`R`.
`-fsign=EBCDIC` is passed to `cobc` so that `S9` fields on disk use the
mainframe convention.  `tests/golden/layouts.py` encodes and decodes the same
convention.

### 3.5 Candidate clock

The Spring Batch candidate stamps `TRAN-PROC-TS` from the JVM clock.
`faketime -f "@2026-03-15 12:00:00 x0"` (rate `x0`) freezes it; without `x0`
the fraction advanced during the run and every `TRANSACT` record differed in
`TRAN-PROC-TS`.  This is recorded here because it is exactly the kind of
difference a reconciliation must be able to attribute: a clock, not logic.

## 4. Candidate finding (pull request #9), as the comparator reported it

Both sets: every `TRANSACT`, `DALYREJS`, `ACCTFILE` and `TCATBALF` record
byte-identical, all control totals equal, `RETURN-CODE` expected `4`, candidate
`0`; verdict `MISMATCH`, exit `1`.  Reports:
`candidate-pr9/named/reconciliation.md`, `candidate-pr9/volume/reconciliation.md`.
The candidate was not modified.
