# Architecture decision records: CardDemo COBOL/CICS → Java 21

Rules every Java ticket applies. Code lives in `modernization/` (see its README).

| ADR | Rule |
| --- | --- |
| [ADR-0001](ADR-0001-modular-monolith.md) | One Spring Boot application, domain packages enforced by ArchUnit |
| [ADR-0002](ADR-0002-paragraph-to-method.md) | COBOL paragraph → camelCase Java method, original name in Javadoc |
| [ADR-0003](ADR-0003-pic-x-to-string.md) | `PIC X(n)` → trimmed `String` with maximum length validation |
| [ADR-0004](ADR-0004-numeric-to-bigdecimal.md) | `PIC S9(n)V99` / `COMP-3` → `BigDecimal` with explicit scale |
| [ADR-0005](ADR-0005-rounding.md) | `RoundingMode.HALF_UP` only where COBOL says `ROUNDED`; truncate otherwise |
| [ADR-0006](ADR-0006-level-88-to-enum.md) | Level-88 condition names → enum |
| [ADR-0007](ADR-0007-commarea-to-session-context.md) | COMMAREA → authenticated session context |
| [ADR-0008](ADR-0008-xctl-link.md) | `EXEC CICS XCTL` → controller dispatch; `EXEC CICS LINK` / `CALL` → service call |
| [ADR-0009](ADR-0009-cics-resp-to-http.md) | CICS RESP conditions → HTTP status |
| [ADR-0010](ADR-0010-read-update-optimistic-locking.md) | `READ UPDATE` / `REWRITE` → `@Transactional` + `@Version` optimistic locking |
| [ADR-0011](ADR-0011-vsam-to-relational.md) | VSAM KSDS → table with primary key; AIX → index |
| [ADR-0012](ADR-0012-gdg-to-dated-storage.md) | GDG → dated rows / dated files |
| [ADR-0013](ADR-0013-cee3abd-to-abend-exception.md) | `CALL 'CEE3ABD'` → unchecked `AbendException` with the original code |
| [ADR-0014](ADR-0014-baseline-clock-pin.md) | Reproduce the baseline clock pin and JCL date parameters in Java |
| [ADR-0015](ADR-0015-batch-harness-return-codes.md) | Batch CLI `--job=`, DD statements as job parameters (file path or `table`), JCL RCs 0/4/8/12/16 in `batch_run` and as the process exit code, `COND=` chaining |
| [ADR-0016](ADR-0016-nightly-cycle-flow-job.md) | Control-M/CA-7 schedule → one `nightly-cycle` Spring Batch flow job (members in baseline order, scheduler conditions as `COND=(4,LT,pred)`, MAXCC), in-app cron off under `test`/`golden` |
| [ADR-0017](ADR-0017-stateless-jwt-session.md) | Sign-on issues a stateless HS256 JWT (`sub` = user id, `role` = `ADMIN`/`USER`) from `CARDDEMO_JWT_SECRET`; online API under `com.carddemo.web`, which domains never depend on |
| [ADR-0018](ADR-0018-plaintext-password-compatibility.md) | USRSEC passwords stay plain text and are compared like COBOL (upper-cased `PIC X(08)`) until a hashing migration; follow-up recorded |
| [ADR-0019](ADR-0019-uniform-online-error-body.md) | Online API errors: RFC 7807 body with `code` / `field` / `message` (exact COBOL text); `NavigationContext` replaces COMMAREA navigation fields |
| [ADR-0020](ADR-0020-pan-masking.md) | Card numbers masked (last four) in card lists and logs, full PAN only on the single-card screens; opaque AES-GCM `cardRef`; a USER only sees cards of the account in context |
| [ADR-0021](ADR-0021-async-report-requests.md) | CORPT00C's internal-reader submit → the same `tranrept` stream on a bounded in-process queue; `report_request` (V5) execution id, status poll and exact report download; start ≤ end enforced |
| [ADR-0022](ADR-0022-react-ui.md) | React + Vite SPA, one route per BMS map, routed by `NavigationContext.toProgram`; JWT + navigation in `sessionStorage`; masked PAN / opaque `cardRef`; nginx serves it and proxies `/api/`, `/v3/`, `/swagger-ui*` to the app |

New ADRs: next free number, same headings (Status/Applies to, Context, Decision). Supersede, do not edit, an accepted ADR.
