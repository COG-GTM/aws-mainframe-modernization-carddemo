# ADR-0006: Level-88 condition names → enum

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Decision
- A field whose values are named by level-88s becomes a Java `enum`; each 88 becomes a constant carrying the
  COBOL value: `CDEMO-USR-TYPE PIC X(01)` with `88 CDEMO-USRTYP-ADMIN VALUE 'A'` / `88 CDEMO-USRTYP-USER VALUE 'U'`
  → `enum UserType { ADMIN("A"), USER("U") }` with `code()` and `static UserType fromCode(String)`.
- `fromCode` on an unknown value throws `InvalidRequestException` (the COBOL `WHEN OTHER` path), unless the program
  explicitly tolerates it, in which case add an `UNKNOWN` constant and document it.
- JPA persists the COBOL code, not the ordinal or name, via an `AttributeConverter`; JSON uses the enum name.
- `SET X TO TRUE` → assignment; `IF X` → `== Enum.X`. Multi-value 88s (`VALUE 'A' 'B'`, `THRU`) become a method
  on the enum (`isActive()`), not extra constants.
- Pure flags (`88 WS-EOF VALUE 'Y'`) on working-storage switches become `boolean`s or disappear in structured
  control flow; they do not need an enum.
- Program state 88s such as `CDEMO-PGM-ENTER VALUE 0` / `CDEMO-PGM-REENTER VALUE 1` are replaced by the HTTP
  method (GET first display, POST re-entry) per ADR-0007.
