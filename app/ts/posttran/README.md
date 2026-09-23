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

## Record layouts

| Module | Copybook | Length | Key |
|---|---|---|---|
| `records/dailyTransaction.ts` | `CVTRA06Y` (`DALYTRAN`) | 350 | none (sequential) |
| `records/transactionMaster.ts` | `CVTRA05Y` (`TRANFILE`) | 350 | `TRAN-ID` X(16) |
| `records/cardCrossReference.ts` | `CVACT03Y` (`XREFFILE`) | 50 | `XREF-CARD-NUM` X(16) |
| `records/account.ts` | `CVACT01Y` (`ACCTFILE`) | 300 | `ACCT-ID` 9(11) |
| `records/transactionCategoryBalance.ts` | `CVTRA01Y` (`TCATBALF`) | 50 | acct 9(11) + type X(02) + cat 9(04) |
| `records/rejectRecord.ts` | in-program (CBTRN02C l.176-182) | 430 | none (sequential) |

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
