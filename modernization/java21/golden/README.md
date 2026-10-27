# GnuCOBOL golden master

Check the committed baseline from the repository root (never modifies expected/):

```sh
modernization/java21/golden/run_golden.sh
```

Prerequisites: GnuCOBOL **3.1** (recorded build: 3.1.2.0), BDB indexed
handler (`cobc --info`), Python 3, GCC, Bash and `faketime`.
On Ubuntu: `sudo apt-get install -y gnucobol faketime`.
No sample datasets are used. All identifiers and data are synthetic; descriptive
identity fields are blank. The fixed generator seed is **104**.

The runner compiles the unmodified `app/cbl/CBTRN02C.cbl` and
`app/cbl/CBACT04C.cbl`, using `app/cpy` and **`-fsign=EBCDIC`**.
This is ASCII text with EBCDIC-style trailing overpunch, not a conversion of
the entire record to EBCDIC. The small `interest_driver.cbl` supplies the
mainframe-style two-byte binary length followed by PARM-DATE `2022071800`.
The clock is frozen, not advancing: **`faketime -f '2022-07-18 12:34:56'`**,
with `TZ=UTC` and `LC_ALL=C`. Both timestamp fields are checked against
`2022-07-18-12.34.56.000000` where generated.

## Starting state and DD roles

Each scenario has its own freshly loaded BDB files. `initial.*.bin` records
the complete starting state; `DALYTRAN.bin` is the complete daily input.
Records are fixed length, with no newline or length prefixes.

| DD | Role and initial state |
| --- | --- |
| ACCTFILE | Indexed 300-byte accounts: four in mixed, 3,000 in volume. Current balance and cycle credit/debit are zero; group SYNTH. Limit 100,000.00 and expiry 2030-01-01 except mixed account 2 limit 1.00 and account 3 expiry 2022-07-17. |
| XREFFILE | Indexed 50-byte card xrefs, one per account; mixed adds card/account 999 with no account row. Primary card key and alternate account key reproduce CBACT04C's lookup. |
| TCATBALF | Indexed 50-byte categories: account 1, type 01, categories 0001 (+100.01), 0002 (-100.01), 0003 (zero). All other category keys absent. |
| DISCGRP | Indexed 50-byte DEFAULT rates for type 01, categories 0001/0002/0003, each 12.50%. SYNTH keys deliberately absent. |
| TRANFILE | Indexed 350-byte TRANSACT master contains one `PRIOR_SEED` transaction, amount 0.07, at start. **The harness does not empty it before posting.** CBTRN02C itself uses OPEN OUTPUT (0100-TRANFILE-OPEN), which discards the seed. The verifier explicitly checks this behavior. |
| DALYTRAN | Sequential 350-byte daily records: mixed seven, volume 3,000. |
| DALYREJS | Absent initially; posting creates sequential 430-byte rejects. |
| TRANSACT | Absent initially; interest creates sequential 350-byte SYSTRAN output. This is distinct from posting's indexed TRANFILE. |

Assignments use `DD_<ASSIGN-name>` environment variables, following
`app/jcl/POSTTRAN.jcl` then `app/jcl/INTCALC.jcl`. CBACT04C consumes exactly
the post-posting ACCTFILE/TCATBALF state, not reloaded fixtures. Its declared
alternate account key is built into XREFFILE; the JCL XREFFIL1 path has no
separate SELECT in this source.

## Case inventory and fixture-to-rule map

| Scenario / named case | Rule and actual source paragraphs | Verification |
| --- | --- | --- |
| mixed / CLEAN_EXISTING | Clean post, 1500-VALIDATE-TRAN, 2000-POST-TRANSACTION, 2700-B-UPDATE-TCATBAL-REC | +1.00 updates account 1/category 1 from 100.01 to 101.01; transaction preserved except processing timestamp. |
| mixed / CLEAN_NEW | 2700-A-CREATE-TCATBAL-REC | +100.01 creates account 4/category 1. |
| mixed / XREF_MISS | 100, 1500-A-LOOKUP-XREF | Card 998 absent; reject trailer 0100. |
| mixed / NO_ACCOUNT | 101, 1500-B-LOOKUP-ACCT | Card 999 exists, account absent; trailer 0101. |
| mixed / OVER_LIMIT | 102, 1500-B-LOOKUP-ACCT | 1.01 exceeds account 2 limit 1.00; trailer 0102. |
| mixed / AFTER_EXPIRY | 103, 1500-B-LOOKUP-ACCT | Account 3 transaction dated after expiry; trailer 0103. |
| mixed / EXPIRY_OVERRIDES | 103 overriding 102, 1500-B-LOOKUP-ACCT | 100,000.01 exceeds limit and is after expiry; trailer 0103. Mixed ends with RC 4. |
| mixed / POSITIVE_TRUNCATION | 1300-COMPUTE-INTEREST, 1300-B-WRITE-TX | 101.01 * 12.50 / 1200 truncates to 1.05; account 4's 100.01 produces 1.04. |
| mixed / NEGATIVE_TRUNCATION | 1300-COMPUTE-INTEREST | -100.01 produces -1.04, toward zero rather than rounding. |
| mixed / ZERO_BALANCE | 1300-COMPUTE-INTEREST | Zero balance with nonzero rate emits a zero-amount transaction. |
| mixed / DEFAULT_FALLBACK | 1200-GET-INTEREST-RATE, 1200-A-GET-DEFAULT-INT-RATE | Missing SYNTH rate falls back to DEFAULT; SYSOUT fallback lines and exact interest amounts checked. |
| mixed / MULTI_CATEGORY | 1050-UPDATE-ACCOUNT | Three categories on account 1 sum to 0.01 interest and update its balance to 1.01; cycles reset. |
| volume / V000000000000001..V000000000003000 | Posting and interest paragraphs above | **3,000** daily records, one per account, seeded signed cent amounts in [-9999,9999]. All post; 3,002 categories generate interest records. Every category balance, interest amount and account outcome is checked. |

These are golden observations, not corrections of legacy behavior. In particular,
the final account's interest transaction is emitted but its account balance and
cycles are **not updated**. `1000-TCATBALF-GET-NEXT` sets END-OF-FILE to Y;
the main PERFORM UNTIL exits before the ELSE containing 1050-UPDATE-ACCOUNT
can execute. Both scenarios assert this final-account behavior explicitly.

## Outputs and byte-identical regeneration

`expected/<scenario>/` contains all input snapshots, each program's indexed
snapshots, DALYREJS, SYSTRAN, unfiltered DISPLAY stdout, stderr (empty),
return codes and assertion summary. `metadata.json` records the compiler,
source SHA-256, clock, parameter and generator settings. Return codes:
mixed CBTRN02C=4, CBACT04C=0; volume CBTRN02C=0, CBACT04C=0.

BDB containers are deliberately not golden artifacts: internal pages and
metadata need not be byte-stable. A generated COBOL bridge loads fixed records
and dumps every indexed file by ascending primary key into `.bin` sequential
files (sizes above). It does not decode/reformat fields or include BDB metadata.
Thus byte comparison covers all record fields including filler and sign bytes.
Build executables, generated bridge source and BDB files stay in ignored
`.build/`. Every run deletes only its own `.build/` and generates into
`.build/generated/`, running all fixture assertions there first. Default CHECK
mode then compares the complete file set and every byte against `expected/`.
Missing, extra and differing files are named and cause a nonzero exit.
CHECK also validates `acceptance.sha256` against the complete baseline.
It never changes `expected/`, `acceptance.sha256` or `app/`.

For an intentional baseline change, explicitly update after verification:

```sh
modernization/java21/golden/run_golden.sh --update
```

Only after both scenarios verify successfully, UPDATE replaces `expected/`
and regenerates `acceptance.sha256` with repository-relative paths. Review and
commit both together. Use the same sources and compiler for identical metadata.

Acceptance procedure in a fresh clone of this branch (Bash):

```sh
modernization/java21/golden/run_golden.sh
modernization/java21/golden/run_golden.sh
git status --porcelain  # empty
modernization/java21/golden/run_golden.sh --update
first=$(cat modernization/java21/golden/acceptance.sha256)
modernization/java21/golden/run_golden.sh --update
diff -u <(printf '%s\n' "$first") modernization/java21/golden/acceptance.sha256
sha256sum -c modernization/java21/golden/acceptance.sha256
git status --porcelain  # empty: both updates match the committed baseline
```

Negative test: flip one byte in `expected/mixed/SYSTRAN.bin` in the scratch
clone, run default CHECK and require nonzero exit naming `mixed/SYSTRAN.bin`.
Restore the original bytes afterward. CHECK must leave the altered baseline
untouched. Also test an extra or missing baseline file: both must fail and
name the affected file.

```sh
python3 - <<'PY'
from pathlib import Path
import subprocess
p = Path('modernization/java21/golden/expected/mixed/SYSTRAN.bin')
original = p.read_bytes()
try:
    p.write_bytes(bytes([original[0] ^ 1]) + original[1:])
    result = subprocess.run(['modernization/java21/golden/run_golden.sh'])
    assert result.returncode != 0
    assert p.read_bytes() != original
finally:
    p.write_bytes(original)
PY
git status --porcelain  # empty
```

Use the same source files and compiler version for byte-identical metadata.
`golden.py` fails immediately on unexpected program RC, stderr, reject reasons,
posting records, overpunch values, timestamps, category or account balances,
interest truncation, or changes to the files interest should leave untouched.
