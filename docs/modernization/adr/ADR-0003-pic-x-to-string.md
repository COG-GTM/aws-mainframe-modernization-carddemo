# ADR-0003: `PIC X(n)` → trimmed `String` with maximum length validation

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
- An alphanumeric field `PIC X(n)` maps to `String`. Inbound values (REST, files) are right-trimmed of spaces;
  an all-space field becomes the empty string, not `null`, unless the column is documented as optional.
- Length is validated at the boundary with `@Size(max = n)` on request DTOs and `@Column(length = n)` on entities,
  where `n` comes from the copybook (`SEC-USR-ID PIC X(08)` → `@Size(max = 8)`, `@Column(length = 8)`).
- Fixed-width files (golden set, DALYTRAN input, reports) are read and written by a record codec that right-pads to
  `n` and fails on overflow instead of truncating silently (pattern harvested from `carddemo-recordio`).
- Comparisons that COBOL does on the padded value (`IF WS-USER-ID = SPACES`, `LOW-VALUES`) become `isBlank()`.
  Case is preserved; upper-casing happens only where the COBOL used `FUNCTION UPPER-CASE` or `INSPECT ... CONVERTING`.
- `PIC 9(n)` keys that are really identifiers (account id `PIC 9(11)`, card number `PIC X(16)`) keep their width:
  zero-padded `String` for display/keys, `long` only where arithmetic is done.
