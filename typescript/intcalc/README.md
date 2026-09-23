# intcalc — TypeScript migration of CBACT04C / job INTCALC

A standalone, runnable TypeScript equivalent of the CardDemo monthly interest
calculation batch job: COBOL program `app/cbl/CBACT04C.cbl`, driven by JCL job
`app/jcl/INTCALC.jcl`.

The program reads the transaction category balance file sequentially, computes
monthly interest per category from the disclosure group rates, writes one
interest transaction per category balance to a new 350-byte `SYSTRAN` file, and
rewrites the account master with the accumulated interest.

Behaviour is specified in `docs/specs/INTCALC-CBACT04C-business-spec.md`
(currently on branch `devin/1790171260-intcalc-business-spec`, PR #30). The
implementation follows that specification, including its documented defects
D1–D12 — this is a faithful migration, not a rewrite.

## Layout

| File | Purpose |
| --- | --- |
| `src/zoned.ts` | `USAGE DISPLAY` zoned-decimal codec (trailing overpunch signs) |
| `src/records.ts` | Fixed-width layouts for TCATBALF / XREFFILE / DISCGRP / ACCOUNT / TRAN |
| `src/io.ts` | File readers, the account store (read + rewrite) and the transaction writer |
| `src/interest.ts` | `(balance × annualRatePercent) / 1200`, truncated toward zero at 2 decimals |
| `src/cbact04c.ts` | The program: account-break loop, rate lookup, account update, transaction build |
| `src/cli.ts` | Command line entry point (the JCL step's DD cards and `PARM`) |

All monetary values use `decimal.js` (`BigDecimal`-equivalent semantics). No
JavaScript `number` arithmetic is used for money or rates.

## Build, test, lint

Requires Node.js 22 or newer.

```bash
npm install
npm test         # vitest: unit + end-to-end tests over app/data/ASCII
npm run lint     # eslint (type-checked rules)
npm run typecheck
npm run build    # emits dist/
```

## Run

```bash
cp ../../app/data/ASCII/acctdata.txt /tmp/acctdata.txt

node dist/cli.js \
  --parm 2022071800 \
  --tcatbal ../../app/data/ASCII/tcatbal.txt \
  --xref ../../app/data/ASCII/cardxref.txt \
  --discgrp ../../app/data/ASCII/discgrp.txt \
  --account /tmp/acctdata.txt \
  --transact-out /tmp/systran.txt
```

Without building, `npm start -- <same arguments>` runs the TypeScript sources
directly.

### Arguments

| Argument | JCL equivalent | Notes |
| --- | --- | --- |
| `--parm <PARM>` | `PARM='2022071800'` | Used **only** as the first 10 bytes of every `TRAN-ID`. Like the COBOL, it is not validated as a date; timestamps come from the wall clock, not from this value. |
| `--tcatbal <file>` | `TCATBALF` | LRECL 50, read sequentially in key order |
| `--xref <file>` | `XREFFILE` | LRECL 50, read by account id (alternate index path) |
| `--discgrp <file>` | `DISCGRP` | LRECL 50, keyed by group + type + category |
| `--account <file>` | `ACCTFILE` | LRECL 300, read and rewritten in place |
| `--transact-out <file>` | `TRANSACT` | LRECL 350, created |
| `--account-out <file>` | — | Write updated accounts elsewhere instead of in place |
| `--line-terminator <t>` | — | `lf` (default), `crlf`, or `none` for a raw `RECFM=F` image |
| `--quiet` | — | Suppress the program's `DISPLAY` output |

Exit codes: `0` success, `2` bad command line, `99` abend (the COBOL's
`CEE3ABD` with code 999, raised when a disclosure group row is missing even for
`DEFAULT`, or when an account or cross-reference record cannot be read).

## Record formats

The copybooks in this job use `USAGE DISPLAY` zoned decimal with **trailing
overpunch sign characters** — there is no COMP-3 anywhere in `CBACT04C`. The
last byte of a signed field encodes both the final digit and the sign:

| Digit | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| positive | `{` | `A` | `B` | `C` | `D` | `E` | `F` | `G` | `H` | `I` |
| negative | `}` | `J` | `K` | `L` | `M` | `N` | `O` | `P` | `Q` | `R` |

So `S9(04)V99` image `00150{` decodes to `15.00`, and `00150}` to `-15.00`.
Field positions, record lengths, leading zeros and blank padding are preserved
exactly, so output records are byte-compatible with the mainframe files.

## Reproduced COBOL defects

The migration deliberately reproduces the observable behaviour of the COBOL,
including its defects. Each of these is pinned by a test:

- **D1 — the last account of every run is never updated.** The COBOL's final
  `1050-UPDATE-ACCOUNT` call sits after `GO TO 1000-EXIT` and is unreachable, so
  the interest accrued for the last account id in the file is written to
  `SYSTRAN` but never added to the account balance, and that account's cycle
  buckets are never cleared. See `runCbact04c` and the D1 tests.
- Missing disclosure group rows fall back to group `DEFAULT`; a missing
  `DEFAULT` row abends the run with `SYSTRAN` discarded and earlier account
  rewrites already committed.
- A zero rate produces neither interest nor a transaction.
- Interest is truncated toward zero per category *before* accumulation, never
  rounded half-up and never truncated on the sum.
- Transaction timestamps use the current wall clock rather than the run-date
  `PARM`.
- `1400-COMPUTE-FEES` remains an explicit, documented no-op extension point
  (`computeFees` in `src/cbact04c.ts`). No fee logic has been invented.

Fixing any of these — D1 above all — is a business decision and is deliberately
out of scope for the migration.
