# Reconciliation checks

Implemented in `test-harness/reconcile.py`; every check is written to
`golden-files/<JOB>/reconciliation.json` with `id`, `group`, `description`,
`expected`, `actual`, `status`. The suite can be pointed at any directory
holding the same JSON files, so the same checks judge a future port:

```bash
python3 test-harness/reconcile.py cbact01c --golden-dir golden-files/CBACT01C
python3 test-harness/reconcile.py cbtrn01c --golden-dir golden-files/CBTRN01C
python3 test-harness/reconcile.py cbtrn01c --golden-dir golden-files/CBTRN01C/synthetic-rejections
```

Exit status is 0 when every check passes, 1 otherwise. All numeric work uses
`decimal.Decimal` on the decimal strings produced by `records.py`, never
floats.

## CBACT01C (`golden-files/CBACT01C`)

Inputs: `input-acctdata.json` (50 accounts), `outfile.json`, `arryfile.json`,
`vbrcfile.json`.

### Record counts

| id | check |
|----|-------|
| `CBACT01C-COUNT-01` | input accounts = OUTFILE records |
| `CBACT01C-COUNT-02` | input accounts = ARRYFILE records |
| `CBACT01C-COUNT-03` | input accounts = VBRCFILE records / 2, and the VBRCFILE count is even |
| `CBACT01C-COUNT-04` | VBRCFILE holds exactly one 12-byte `VBRC-REC1` and one 39-byte `VBRC-REC2` per account, strictly alternating REC1, REC2 |

### Field totals

| id | check |
|----|-------|
| `CBACT01C-TOTAL-ACCT-CURR-BAL` | Σ `ACCT-CURR-BAL` (in) = Σ `OUT-ACCT-CURR-BAL` |
| `CBACT01C-TOTAL-ACCT-CREDIT-LIMIT` | Σ `ACCT-CREDIT-LIMIT` (in) = Σ `OUT-ACCT-CREDIT-LIMIT` |
| `CBACT01C-TOTAL-ACCT-CASH-CREDIT-LIMIT` | Σ `ACCT-CASH-CREDIT-LIMIT` (in) = Σ `OUT-ACCT-CASH-CREDIT-LIMIT` |
| `CBACT01C-TOTAL-ACCT-CURR-CYC-CREDIT` | Σ `ACCT-CURR-CYC-CREDIT` (in) = Σ `OUT-ACCT-CURR-CYC-CREDIT` |
| `CBACT01C-TOTAL-CYC-DEBIT` | Σ `OUT-ACCT-CURR-CYC-DEBIT` (COMP-3) = Σ over input of (`2525.00` if `ACCT-CURR-CYC-DEBIT` = 0 else `ACCT-CURR-CYC-DEBIT`). All 50 sample accounts have a zero debit, so the expected total is 50 × 2525.00 = 126250.00 |
| `CBACT01C-TOTAL-ARR-ARR-ACCT-CURR-CYC-DEBIT-1` | Σ `ARR-ACCT-CURR-CYC-DEBIT(1)` = records × 1005.00 |
| `CBACT01C-TOTAL-ARR-ARR-ACCT-CURR-CYC-DEBIT-2` | Σ `ARR-ACCT-CURR-CYC-DEBIT(2)` = records × 1525.00 |
| `CBACT01C-TOTAL-ARR-ARR-ACCT-CURR-BAL-3` | Σ `ARR-ACCT-CURR-BAL(3)` = records × −1025.00 |
| `CBACT01C-TOTAL-ARR-ARR-ACCT-CURR-CYC-DEBIT-3` | Σ `ARR-ACCT-CURR-CYC-DEBIT(3)` = records × −2500.00 |
| `CBACT01C-TOTAL-ARR-ACCT-CURR-BAL-1/-2` | Σ `ARR-ACCT-CURR-BAL(1)` and `(2)` = Σ input `ACCT-CURR-BAL` |
| `CBACT01C-TOTAL-ARR-ZERO-4/-5` | occurrences 4 and 5 are `INITIALIZE`d: both fields are 0.00 in every record |
| `CBACT01C-TOTAL-VB2-CURR-BAL`, `-VB2-CREDIT-LIMIT` | Σ `VB2-ACCT-CURR-BAL` / `VB2-ACCT-CREDIT-LIMIT` = Σ input |

### Derived fields (per record)

| id | check |
|----|-------|
| `CBACT01C-FIELD-REISSUE-DATE` | `OUT-ACCT-REISSUE-DATE` = input `ACCT-REISSUE-DATE` with the hyphens removed (`COBDATFT` type 2 → 2) followed by two spaces (the 8-byte result lands in a 10-byte field) |
| `CBACT01C-FIELD-VB2-YYYY` | `VB2-ACCT-REISSUE-YYYY` = first 4 characters of input `ACCT-REISSUE-DATE` |
| `CBACT01C-FIELD-VB1-STATUS` | `VB1-ACCT-ACTIVE-STATUS` = input `ACCT-ACTIVE-STATUS` |

### Cross-reference integrity

| id | check |
|----|-------|
| `CBACT01C-XREF-00` | input `ACCT-ID` is unique (it is the KSDS primary key) |
| `CBACT01C-XREF-01..04` | every `OUT-ACCT-ID`, `ARR-ACCT-ID`, `VB1-ACCT-ID`, `VB2-ACCT-ID` exists exactly once in the input, and every input account appears exactly once (reports `unknown`, `duplicated`, `missing` lists) |
| `CBACT01C-XREF-05` | OUTFILE order = input key order (a sequential read of a KSDS is ascending by key) |

## CBTRN01C (`golden-files/CBTRN01C` and `.../synthetic-rejections`)

Inputs: `input-dailytran.json`, `outcomes.json`, `input-cardxref.json`,
`input-acctdata.json`, and (optionally) `display.txt` for the lookup count.

Outcome classes, derived from the DISPLAY stream of the real program:

* `VERIFIED` – `SUCCESSFUL READ OF XREF` then `SUCCESSFUL READ OF ACCOUNT FILE`
* `CARD_NOT_FOUND` – `INVALID CARD NUMBER FOR XREF` then
  `CARD NUMBER <n> COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-<id>`
* `ACCOUNT_NOT_FOUND` – xref found, then `INVALID ACCOUNT NUMBER FOUND` and
  `ACCOUNT <id> NOT FOUND`

### Record counts

| id | check |
|----|-------|
| `CBTRN01C-COUNT-01` | daily transactions in = outcome rows out |
| `CBTRN01C-COUNT-02` | verified + card-missing + account-missing = total |
| `CBTRN01C-COUNT-03` | outcome rows are in file order; row *i* has the `DALYTRAN-ID` and card of input record *i* |
| `CBTRN01C-COUNT-04` | XREF lookups in `display.txt` = transactions + 1. Legacy quirk: `1000-DALYTRAN-GET-NEXT` sets the EOF flag but only guards the DISPLAY, so the last record is looked up once more after end-of-file. A port must either reproduce this line or document the divergence (see TEST_STRATEGY.md) |

### Field totals

| id | check |
|----|-------|
| `CBTRN01C-TOTAL-01` | Σ `DALYTRAN-AMT` over the input = Σ of the per-class totals |
| `CBTRN01C-TOTAL-02` | Σ `DALYTRAN-AMT` per outcome class (`VERIFIED`, `CARD_NOT_FOUND`, `ACCOUNT_NOT_FOUND`) – recorded so a port's split must match |
| `CBTRN01C-TOTAL-03` | debit/credit split: Σ positive amounts, Σ negative amounts, count of zero amounts |

### Cross-reference integrity

| id | check |
|----|-------|
| `CBTRN01C-XREF-01` | every `VERIFIED` card exists in cardxref and its `XREF-ACCT-ID` exists in acctdata; the outcome's `acct_id` equals that `XREF-ACCT-ID` |
| `CBTRN01C-XREF-02` | every `CARD_NOT_FOUND` card does not exist in cardxref |
| `CBTRN01C-XREF-03` | every `ACCOUNT_NOT_FOUND` card exists in cardxref but its account is not in acctdata |
| `CBTRN01C-XREF-04` | coverage: number of distinct cards referenced by the transactions |

The shipped sample data resolves all 300 transactions (`VERIFIED` = 300,
others 0). `golden-files/CBTRN01C/synthetic-rejections` is a 3-record
fixture (built by `cobol/run_synthetic_rejections.sh` from the sample data)
that exercises the other two branches, so `XREF-02` and `XREF-03` are
checked against real program output as well.
