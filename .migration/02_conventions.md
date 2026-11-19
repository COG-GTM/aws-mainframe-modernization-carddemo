# 02 - Conventions

Binding for every PR under `modernization/`. Source of the type rules: REF `docs/migration/conventions.md` §2–§9 (adopted, see D-004/D-005).

## Code
- Packages `com.carddemo.<context>.{domain,repository,service,batch}`; shared code in `carddemo-common` `com.carddemo.common.{cobol,record,batch}` (D-003).
- Business rules live in services, never in readers/writers/entities. Javadoc of each rule method cites `CBTRN02C <paragraph>` and the source line.
- Money: `BigDecimal` scale 2; arithmetic truncates like COBOL unless the source says `ROUNDED`. `double`/`float` forbidden.
- `PIC 9(n)` ids: `long`, formatted back with zero padding to n digits. `PIC X(n)`: `String`, right-padded with spaces on output.
- Dates/timestamps the stream copies or compares as text (`ACCT-EXPIRAION-DATE`, `*-ORIG-TS`, `*-PROC-TS`) stay strings (D-004).
- Abend paths → exception that fails the step (D-006). No silent catch.
- No Lombok, no H2, no new dependency without a decision in `06_decisions.md` (D-009).

## Data
- Flyway `V<n>__<snake_name>.sql`, never edited after merge; schemas per context (D-010); `ddl-auto: validate`.
- KSDS key → primary key; FILLER not stored (D-020).

## Tests
- JUnit 5 + AssertJ; DB tests on Testcontainers `postgres:16` with Flyway.
- Golden tests: oracle `expected/*` files committed under `modernization/carddemo-app/src/test/resources/golden/posttran/<case>/`, compared byte for byte (TRAN-PROC-TS normalized to the pinned clock, see `07_runbook.md` R-05).
- Every rule in the program FR has at least one test named after the requirement id.

## Git / PR
- One branch per stacked PR; PR body lists FR ids covered and oracle cases passed. No merges by Devin.
