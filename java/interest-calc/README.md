# interest-calc — Java port of the CBACT04C interest calculation slice

A plain Java 17 module (no framework dependencies) reproducing the business rules of
`app/cbl/CBACT04C.cbl`, the batch interest calculator driven by `app/jcl/INTCALC.jcl`.

```
mvn test
```

## Layout

| Path | Contents |
|---|---|
| `CobolDecimal` | Fixed-point semantics of `PIC S9(n)V99`: truncation toward zero, high-order digit drop, zoned/overpunched rendering |
| `model/` | One record per copybook (`CVTRA01Y`, `CVTRA02Y`, `CVACT01Y`, `CVACT03Y`, `CVTRA05Y`), field widths preserved |
| `InterestCalculator` | The arithmetic paragraphs: monthly interest, accumulation, account posting, interest transaction |
| `InterestCalculationBatch` | The PROCEDURE DIVISION control-break loop, with the five VSAM files behind ports |
| `Db2Timestamp` | `Z-GET-DB2-FORMAT-TIMESTAMP` |

## Deliberate fidelity choices

- Interest is truncated, not rounded — the COBOL `COMPUTE` carries no `ROUNDED` phrase.
- Amount fields wrap rather than throw on overflow — the COBOL statements carry no `ON SIZE ERROR`.
- A missing disclosure group is retried under group id `DEFAULT`; a missing default abends.
- `computeFees` is a no-op because `1400-COMPUTE-FEES` is an unimplemented stub in the COBOL.
- By default the last account in the file receives no balance update, reproducing an unreachable
  `ELSE` branch in the COBOL loop. Pass `postFinalAccountGroup = true` to correct it.

No mainframe tooling is required to build or run this module; the file ports are implemented by the
tests with in-memory maps, and a production adapter would back them with the replicated database.
