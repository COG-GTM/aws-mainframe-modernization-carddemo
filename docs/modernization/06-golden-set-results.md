# 6. Golden-set equivalence results — posting slice (CBTRN02C)

## 6.1 What was compared

- **Reference**: the unmodified `app/cbl/CBTRN02C.cbl`, compiled in this run with
  `cobc -x -std=ibm -fsign=EBCDIC -I app/cpy -o tests/golden/.work/build/CBTRN02C app/cbl/CBTRN02C.cbl`
  (the exact command recorded in `tests/golden/sets/<set>/expected/run.json`), then executed under
  GnuCOBOL 3.1.2 with the clock frozen at the set's `COB_CURRENT_DATE` (`2026-03-15 12:00:00`).
- **Candidate**: `transaction-posting-service-0.1.0-SNAPSHOT.jar`, built clean (`mvn clean package`)
  in this run from the candidate commit and run unmodified via `faketime -f "@2026-03-15 12:00:00 x0"`.
  File locations, ASCII encoding and the frozen clock are supplied externally; nothing in the
  candidate is edited.
- `compare.py` pairs records by key, compares every field, compares RETURN-CODE as an integer,
  and reconciles control totals (records in / accepted / rejected / by reason / amount sums /
  closing balances).

## 6.2 Command and result

```
$ bash scripts/run-equivalence.sh
```

Output of `python3 tests/golden/equivalence_table.py` (the table step), verbatim:

```
Set    | Dataset     | Records exp              | Records cand             | Amount exp | Amount cand | Byte-identical | Result
-------+-------------+--------------------------+--------------------------+------------+-------------+----------------+-------
named  | TRANSACT    | 14                       | 14                       | 1010.95    | 1010.95     | yes            | PASS
named  | DALYREJS    | 8                        | 8                        | 1170.00    | 1170.00     | yes            | PASS
named  | ACCTFILE    | 17                       | 17                       | 1160.95    | 1160.95     | yes            | PASS
named  | TCATBALF    | 13                       | 13                       | 1070.95    | 1070.95     | yes            | PASS
named  | RETURN-CODE | 4                        | 4                        |            |             | yes            | PASS
named  | CONTROL     | in=22 acc=14 rej=8       | in=22 acc=14 rej=8       |            |             | yes            | PASS
named  | SYSOUT      |                          |                          |            |             | n/a            | INFO
volume | TRANSACT    | 1047                     | 1047                     | 231747.52  | 231747.52   | yes            | PASS
volume | DALYREJS    | 153                      | 153                      | 46628.79   | 46628.79    | yes            | PASS
volume | ACCTFILE    | 167                      | 167                      | 231747.52  | 231747.52   | yes            | PASS
volume | TCATBALF    | 578                      | 578                      | 1153581.00 | 1153581.00  | yes            | PASS
volume | RETURN-CODE | 4                        | 4                        |            |             | yes            | PASS
volume | CONTROL     | in=1200 acc=1047 rej=153 | in=1200 acc=1047 rej=153 |            |             | yes            | PASS
volume | SYSOUT      |                          |                          |            |             | n/a            | INFO

named: compare.py exit=0 (PASS); candidate commit=58d6ee86386d185d4529ffe60ae3138882a76e34, jar_sha256=376ec28a6e7784e9e51e85577d787477f9e6283c56829d67b40941c623ec8bcf, faketime="@2026-03-15 12:00:00 x0", openjdk version "21.0.12.1" 2026-08-18
named: reference cobc (GnuCOBOL) 3.1.2.0, cobc -x -std=ibm -fsign=EBCDIC -I app/cpy -o tests/golden/.work/build/CBTRN02C app/cbl/CBTRN02C.cbl
named: CBTRN02C.cbl sha256 708f3cadc555acab63f11e2f3238f5372ac7180e6b01197bf960d96bf0d2e83f
volume: compare.py exit=0 (PASS); candidate commit=58d6ee86386d185d4529ffe60ae3138882a76e34, jar_sha256=cf25c7b428d7768ebd507df5e31c918072072b71aa91bad25646139b5e021cda, faketime="@2026-03-15 12:00:00 x0", openjdk version "21.0.12.1" 2026-08-18
volume: reference cobc (GnuCOBOL) 3.1.2.0, cobc -x -std=ibm -fsign=EBCDIC -I app/cpy -o tests/golden/.work/build/CBTRN02C app/cbl/CBTRN02C.cbl
volume: CBTRN02C.cbl sha256 708f3cadc555acab63f11e2f3238f5372ac7180e6b01197bf960d96bf0d2e83f

named: PASS
volume: PASS

OVERALL: PASS
```

The candidate commit shown is the commit this run was made from; no file under `modernization/` changed after it (later commits touch only docs and the runner script).

Machine-readable copies: `tests/golden/.work/equivalence-summary.{md,json}`; per-set detail:
`tests/golden/sets/<set>/candidate-pr9/reconciliation.{md,json}`.

## 6.3 What the sets contain

From `tests/golden/sets/<set>/input/manifest.json` (all data is synthetic; card numbers fail Luhn
by construction, account/customer/merchant IDs are `9000`-prefixed, merchant names are
`GOLDEN MERCHANT nnnn`):

- **named** — 22 hand-built edge-case transactions against 17 accounts, 18 xref rows,
  11 category-balance rows. Cases: `clean_accept`, `card_not_in_xref`, `account_not_on_file`,
  `overlimit`, `expired_account`, `zero_amount`, `negative_amount_credit`, `max_amount`,
  `max_negative_amount`, `category_balance_crosses_zero`, `same_account_running_balance`,
  `same_account_two_accepts`, `category_not_in_tcatbal`, `overlimit_and_expired`,
  `expiry_boundary_equal`, `expiry_boundary_day_before`, `debit_sign_interaction`,
  `credit_relieves_overlimit`, `two_categories_one_account`. Result: 14 posted, 8 rejected
  (reasons 0100×1, 0101×1, 0102×3, 0103×3).
- **volume** — 1,200 generated transactions (seed 20260315) across the same case families
  (`vol_clean_accept`, `vol_negative_amount`, `vol_card_not_in_xref`, `vol_account_not_on_file`,
  `vol_overlimit_attempt`, `vol_expired_account`, `vol_zero_amount`), against 167 accounts,
  191 xref rows, 411 category-balance rows. Result: 1,047 posted, 153 rejected
  (0100×49, 0101×33, 0102×52, 0103×19). Every volume account opens at `ACCT-CURR-BAL` 0.00
(opening sum over `sets/volume/input/ACCTFILE` = 0.00), which is why the volume ACCTFILE closing
amount (231747.52) equals the volume TRANSACT amount: posted transaction amounts are the only
credits and debits applied.

## 6.4 What the first run found

The first candidate run produced **one difference** on both sets: RETURN-CODE `4` expected
(CBTRN02C sets `RETURN-CODE 4` whenever `WS-REJECT-COUNT > 0`), `0` from the Java process.
Every record, field, order and control total already matched byte-for-byte.

Root cause: the batch configuration mapped `COMPLETED_WITH_REJECTS` → 4 in a
`SimpleJvmExitCodeMapper` bean, but nothing consumed it — Spring Boot's built-in
`JobExecutionExitCodeGenerator` maps only `BatchStatus` to the process exit code and ignores
the job `ExitStatus` entirely, so a completed job always exited 0. Fixed by
`PostingExitCodeGenerator`, an `Ordered.HIGHEST_PRECEDENCE` `ExitCodeGenerator` fed by the same
mapper, plus `PostingProcessExitCodeTest` covering exit 4 / 0 / non-zero on failure.

This matters because the return code is the only signal the scheduler gets from the step: 4 is
how the existing job tells operators "posted with rejects" apart from "posted clean". A
replacement that exits 0 loses that signal silently, even though every record is right.

## 6.5 How the harness proves it can fail

`tests/golden/selftest.sh` injects defects into copies of the expected outputs and checks the
comparator catches each with the right verdict (exit 1 field mismatch / exit 2 order-only /
exit 3 within tolerance / exit 64 bad usage). Verbatim from the run:

```
checks passed: 45 of 45  (exact-copy x2 + 9 mutants x2 + 4 tolerance-path x2 + 2 absence x2 + 2 sysout-policy x2 + 4 pairing x2 + docs sync)
RESULT: PASS - 18 of 18 injected defects caught; exact copy compares clean; tolerance path 8 of 8; absence 4 of 4; sysout policy 4 of 4; pairing 8 of 8
```

The pre-fix run above is itself a live example: the comparator caught the RETURN-CODE difference.

## 6.6 Scope of the claim

This is equivalence with a **GnuCOBOL 3.1.2 run of the same COBOL source** on these inputs —
not with Enterprise COBOL on z/OS. Not covered: mainframe compiler options and runtime semantics,
EBCDIC-at-rest VSAM datasets (both sides here read/write ASCII flat files with EBCDIC sign
convention), JCL abend paths beyond the exit code, CICS, and production volumes. Section 5.3
lists what would close that gap. SYSOUT is the operator console (COBOL `DISPLAY` vs Spring Boot
log) and is reported as INFO, not compared.

## 6.7 How to re-run

```bash
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
bash scripts/run-equivalence.sh                    # both sets, reference + candidate + table
bash scripts/run-equivalence.sh --set named        # one set
bash scripts/run-equivalence.sh --skip-reference   # skip the GnuCOBOL re-run
GS_MVN_ARGS="-s /path/to/settings.xml" bash scripts/run-equivalence.sh   # Maven mirror
```

The reference step verifies the committed `expected/` outputs still match a fresh GnuCOBOL run in
this environment; the script fails if they drift. Requires `cobc`, `faketime`, `python3`,
JDK 21 and Maven.
