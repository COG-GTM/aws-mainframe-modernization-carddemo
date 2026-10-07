# ADR-0004: `PIC S9(n)V99` / `COMP-3` → `BigDecimal` with explicit scale

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
- Every numeric field with an implied decimal point (`V`), packed or zoned (`PIC S9(10)V99`, `PIC S9(09)V99 COMP-3`),
  maps to `java.math.BigDecimal` with scale equal to the digits after `V`: `setScale(2)` on input, JPA
  `@Column(precision = 12, scale = 2)` for `S9(10)V99` (precision = total digits), Flyway `NUMERIC(12,2)`.
- Integer fields `PIC 9(n)`/`S9(n)` (any usage, incl. `COMP`/`COMP-3`) map to `int` for n ≤ 9, `long` for n ≤ 18.
- Binary floating point (`double`, `float`, wrappers) is forbidden in `com.carddemo` fields (ArchUnit
  `noBinaryFloatingPoint`). Use `BigDecimal.valueOf(long, scale)` or the string constructor, never `new BigDecimal(double)`.
- Overflow: COBOL silently drops high-order digits on `MOVE`/`COMPUTE` without `ON SIZE ERROR`. Java code reproduces
  that only where the baseline shows it matters; otherwise it validates precision and raises `InvalidRequestException`.
  Each such case is recorded in the program's rules file.
- Sign and packed layout (`COMP-3` nibble encoding) live only in the record codec, never in domain code.

Rounding is ADR-0005.
