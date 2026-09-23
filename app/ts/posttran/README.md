# POSTTRAN / CBTRN02C — TypeScript implementation

TypeScript re-implementation of the CardDemo nightly posting job `POSTTRAN`
(COBOL program `app/cbl/CBTRN02C.cbl`), following the approved business
specification `docs/specs/POSTTRAN-CBTRN02C-business-spec.md`.

The implementation is deliberately **bug-for-bug faithful** to the COBOL: where
the mainframe behaviour is surprising, it is reproduced and documented rather
than corrected.

## Toolchain

Node.js >= 22.18 (native TypeScript type stripping) and TypeScript 5.9 in
strict mode. No runtime dependencies.

```bash
npm install
npm run lint       # eslint (type-checked rules)
npm run typecheck  # tsc --noEmit
npm test           # node --test over the TypeScript sources
npm run build      # emits dist/
```

## Layout

| Path | Contents |
|---|---|
| `src/codec/money.ts` | scale-2 decimal money as bigint minor units — never `number` |
| `src/codec/zonedDecimal.ts` | signed zoned decimal (trailing sign overpunch) codec |
| `src/codec/fixedWidth.ts` | 1-based offset readers/writers for fixed-width records |
| `src/records/` | one module per copybook record layout |
| `src/io/` | keyed (VSAM-equivalent) stores and the six DD-name file adapters |
| `src/validation/` | reason codes 100/101/102/103 (`1500-VALIDATE-TRAN`) |
| `src/posting/` | posting, timestamp generation and the main loop |
| `src/cli.ts` | runnable equivalent of job step `POSTTRAN.STEP15` |

## Running the job against the sample data

The job rewrites `ACCTFILE`, `TCATBALF`, `TRANFILE` and `DALYREJS`, so copy the
repository sample data somewhere scratch first:

```bash
mkdir -p out
cp ../../data/ASCII/{dailytran.txt,cardxref.txt,acctdata.txt,tcatbal.txt} out/
: > out/tranfile.txt
: > out/dalyrejs.txt

npm run posttran -- \
  --dalytran=out/dailytran.txt \
  --xreffile=out/cardxref.txt \
  --acctfile=out/acctdata.txt \
  --tcatbalf=out/tcatbal.txt \
  --tranfile=out/tranfile.txt \
  --dalyrejs=out/dalyrejs.txt
```

Against the unmodified sample data this prints
`TRANSACTIONS PROCESSED :000000300` / `TRANSACTIONS REJECTED  :000000038`
(all 38 are reason 102, over limit) and exits with code **4**, matching the
`COND=(4,LT)` convention of the JCL. Exit code 0 means no rejects; exit code 12
stands in for the COBOL `9999-ABEND-PROGRAM` abend.

After `npm run build` the same entry point is `node dist/cli.js` with identical
options.

## Record layouts

| Module | Copybook | Length | Key |
|---|---|---|---|
| `records/dailyTransaction.ts` | `CVTRA06Y` (`DALYTRAN`) | 350 | none (sequential) |
| `records/transactionMaster.ts` | `CVTRA05Y` (`TRANFILE`) | 350 | `TRAN-ID` X(16) |
| `records/cardCrossReference.ts` | `CVACT03Y` (`XREFFILE`) | 50 | `XREF-CARD-NUM` X(16) |
| `records/account.ts` | `CVACT01Y` (`ACCTFILE`) | 300 | `ACCT-ID` 9(11) |
| `records/transactionCategoryBalance.ts` | `CVTRA01Y` (`TCATBALF`) | 50 | acct 9(11) + type X(02) + cat 9(04) |
| `records/rejectRecord.ts` | in-program (CBTRN02C l.176-182) | 430 | none (sequential) |

## DD names and datasets

| DD name | Mainframe dataset | Open mode | TypeScript adapter |
|---|---|---|---|
| `DALYTRAN` | `AWS.M2.CARDDEMO.DALYTRAN.PS` | INPUT (sequential) | array of parsed records |
| `XREFFILE` | `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` | INPUT (random) | `KeyedStore` (read only) |
| `ACCTFILE` | `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` | I-O | `KeyedStore` (read + rewrite) |
| `TCATBALF` | `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` | I-O | `KeyedStore` (read + write + rewrite) |
| `TRANFILE` | `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS` | **OUTPUT** | `KeyedLoader` — replaces the dataset, ascending keys only |
| `DALYREJS` | `AWS.M2.CARDDEMO.DALYREJS(+1)` | OUTPUT | `RejectFile` — 430-byte records |

Keyed files are loaded into memory on open and written back on close, so a run
is all-or-nothing on disk. The COBOL has no unit of work at all (spec §6.4);
this does not change the posted results, only when they hit the filesystem.

## Decimal and sign handling

* Amounts are COBOL zoned decimal `S9(n)V99`: `n+2` bytes where the final byte
  carries the last digit and the sign (`{`/`A`-`I` = +0..+9, `}`/`J`-`R` =
  -0..-9). No packed decimal (`COMP-3`) appears in these copybooks.
* Money is held as `bigint` minor units (scale 2). JavaScript `number` is never
  used for money, and no rounding is applied anywhere — matching the COBOL,
  which only ever `ADD`s 2-decimal operands.
* `PIC 9(n)` fields (keys, merchant id, category code) are kept as fixed-width
  zero-padded **strings** so key padding survives round-tripping.
* Short input lines (the ASCII sample files omit trailing filler) are padded
  with spaces to the record length; `\r\n` line endings are tolerated.

## Faithfully reproduced COBOL quirks

| Behaviour | Where |
|---|---|
| Reason 100 short-circuits the account lookup, so 100 and 101 never co-occur | `validation/validateTransaction.ts` |
| Reason 103 overwrites reason 102 — an expired **and** over-limit transaction reports only 103 | same |
| Over-limit uses `credit limit >= cycle credit - cycle debit + amount`, ignoring `ACCT-CURR-BAL` | `isWithinCreditLimit` |
| Expiry is a 10-character string comparison, not date arithmetic | `isWithinExpiry` |
| `ACCT-ACTIVE-STATUS` is never checked, so closed accounts still post | `validateTransaction` |
| A negative amount is *added* to `ACCT-CURR-CYC-DEBIT`, making it more negative | `applyAccountBalances` |
| Reason 109 (account rewrite failed) is set and never tested: no reject, transaction still written | `updateAccountRecord` |
| The inbound processing timestamp is discarded and regenerated | `buildTransactionMasterRecord` |
| `TRANFILE` is loaded, not appended — the run replaces the dataset | `io/datasets.ts` |

The reason-109 dead logic is isolated in `updateAccountRecord`, and
`runPostingJob` exposes `treatAccountRewriteFailureAsReject` (default `false` =
COBOL behaviour) as the single, tested seam for correcting it later.
