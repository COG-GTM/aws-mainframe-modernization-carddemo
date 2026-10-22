# Batch transaction posting: CBTRN01C, CBTRN02C, CBTRN03C

TypeScript ports of `app/cbl/CBTRN01C.cbl`, `CBTRN02C.cbl` (POSTTRAN) and
`CBTRN03C.cbl` (TRANREPT). Each program is a function that takes a
`VsamCatalog` (see `loadSeedCatalog()`) plus file paths for the sequential DDs
and returns `{ returnCode, abended, sysout }`.

| Program | Module | Entry |
|---|---|---|
| CBTRN01C | `src/batch/cbtrn01c/` | `cbtrn01c({ catalog, dalytran })` |
| CBTRN02C | `src/batch/cbtrn02c/` | `cbtrn02c({ catalog, dalytran, dalyrejs, now? })` -> also `transactionCount`, `rejectCount` |
| CBTRN03C | `src/batch/cbtrn03c/` | `cbtrn03c({ catalog, tranfile, dateparm, tranrept })`; `tranreptSortInput()` = TRANREPT STEP05R SORT |
| shared | `src/batch/cbtrn-common/` | DISPLAY capture, 9910/9999 paragraphs, `truncateTo`, CURRENT-DATE, `PrimeKeyOnlyIndexedFile` |

```ts
const { catalog } = loadSeedCatalog();
const post = cbtrn02c({ catalog, dalytran: 'app/data/ASCII/dailytran.txt', dalyrejs: '/tmp/dalyrejs.txt' });
// post.transactionCount 300, post.rejectCount 38 (all 102), post.returnCode 4; TRANFILE/ACCTFILE/TCATBALF updated in `catalog`
```

Behaviour reproduced as coded, including the quirks:

- **CBTRN01C**: DISPLAY only, RC 0. After end of file the XREF/ACCOUNT lookups
  run once more with the last record read. 9000-DALYTRAN-CLOSE reports
  `ERROR CLOSING CUSTOMER FILE`.
- **CBTRN02C**: reject codes 100 (no XREF), 101 (no account), 102 (overlimit:
  `ACCT-CREDIT-LIMIT >= CYC-CREDIT - CYC-DEBIT + AMT` into `WS-TEMP-BAL
  S9(09)V99`, high-order truncation included), 103 (expired, overrides 102).
  Posting order TCATBAL (create-or-update), ACCOUNT, then TRANSACT WRITE, so a
  duplicate TRAN-ID abends (status 22, RC 999) after the balances were already
  updated. A created TCATBAL record keeps the FILLER of the last record read
  (INITIALIZE skips FILLER). RC 4 when any reject.
- **CBTRN03C**: the first record outside the date range ends the run with no
  totals (`ELSE NEXT SENTENCE` jumps past `END-PERFORM.`). At end of file the
  last amount is added a second time to the page/grand totals and the last
  account total is never written. Abends (status 23) on an unknown card, type
  or category.

## Testing

- `test/batch/cbtrn/golden.test.ts`: parity with the original COBOL run under
  GnuCOBOL 3.1 (`tools/cbtrn/gen-golden-cbtrn.sh`, needs `cobc`; indexed files
  stand in for VSAM, TRAN-PROC-TS masked). Compared: SYSOUT, RC, posted
  TRANSACT, ACCTDATA, TCATBAL, DALYREJS for CBTRN02C (sample, crafted reject
  codes 100/101/103 + debit + TCATBAL create, duplicate-key abend); SYSOUT for
  CBTRN01C; report + SYSOUT for CBTRN03C (sorted, unfiltered/out-of-range, empty).
- `test/batch/cbtrn0{1,2,3}c/`: unit tests for the traced edge cases.

## LIMITATIONS

- **CURRENT-DATE**: TRAN-PROC-TS comes from `now()` (local time, hundredths);
  golden files mask it because GnuCOBOL ignores `COB_CURRENT_DATE` for it.
- **CEE3ABD** is not called; an abend returns `{ returnCode: 999, abended: true }`.
  GnuCOBOL's stub exits with 999 mod 256 = 231, which the golden test accounts for.
  Files are closed implicitly; records already written stay written.
- **DISPLAY of signed numerics** (CBTRN03C `TRAN-AMT` / `WS-PAGE-TOTAL` debug
  lines) uses the GnuCOBOL form `00000076566+`; z/OS shows the overpunched
  form `0000007656F`.
- **Status 02**: `IndexedFile` reports 02 for duplicate keys of any cluster AIX
  (TRANAIX on TRANSACT); these programs declare no ALTERNATE RECORD KEY, so
  `PrimeKeyOnlyIndexedFile` reports 00 instead, as VSAM does for them.
- **Uncaught file errors**: an I/O failure outside class 2 on a keyed READ /
  REWRITE without a status check (no USE declaratives) is ignored here; the
  COBOL runtime would terminate. Not reachable with the in-memory store.
- **Collation**: date-range and expiry comparisons use ASCII byte order
  (z/OS: EBCDIC). They only differ for non-digit dates.
- **Sequential DDs** default to LINE files (the ASCII seeds); pass
  `*Organization: 'FIXED'` for RECFM=FB images. `tranreptSortInput` treats
  non-digit card-number bytes as 0 for the ZD sort key.
