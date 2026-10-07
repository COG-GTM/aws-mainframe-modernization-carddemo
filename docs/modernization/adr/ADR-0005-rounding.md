# ADR-0005: `RoundingMode.HALF_UP` only where COBOL says `ROUNDED`; truncate otherwise

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
COBOL `COMPUTE`/`DIVIDE`/`MULTIPLY` without `ROUNDED` truncates toward zero to the receiving field's scale.
`grep ROUNDED app/cbl/*.cbl` finds no occurrence in the core app, so today every result truncates, e.g. CBACT04C
`COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200` into `S9(09)V99`.

## Decision
- Without `ROUNDED`: compute at full precision, then `setScale(targetScale, RoundingMode.DOWN)`
  (`DOWN` = toward zero, which is COBOL truncation for negatives too; not `FLOOR`).
- With `ROUNDED`: `setScale(targetScale, RoundingMode.HALF_UP)`.
- Intermediate results: do not round intermediates. Divide with enough working scale
  (`MathContext.DECIMAL128` or a scale ≥ target + 10) and apply the single final `setScale`; this matches the
  GnuCOBOL baseline (`-std=ibm`), which the golden set checks.
- The method Javadoc cites the COBOL statement so reviewers can see which mode applies.
- `HALF_EVEN`, `BigDecimal.divide` without a scale/rounding mode, and `double` math are not allowed.
