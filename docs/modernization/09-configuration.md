# 09 — Configuration reference

Every setting of `modernization/carddemo-app`: Spring profiles, the `carddemo.*` properties, the `CARDDEMO_*`
environment variables that feed them, and the batch command-line conventions (DD parameters, file vs table mode,
dated generations). Sources: `src/main/resources/application*.yml`, the `@ConfigurationProperties` records listed
below, `modernization/docker-compose.yml` and `modernization/.env.example`.

## 1. Spring profiles

| Profile | Activated by | What it changes |
| --- | --- | --- |
| (none) | always | `application.yml`: datasource from `CARDDEMO_DB_*`, Flyway `classpath:db/migration`, Spring Batch never launches at start-up, actuator `health,info`, springdoc paths, every `carddemo.*` default below. `CARDDEMO_DB_PASSWORD` and `CARDDEMO_JWT_SECRET` have **no default**: the web app does not start without the JWT key. |
| `local` | docker compose (`CARDDEMO_PROFILES`, default `local`), `mvn spring-boot:run -Dspring-boot.run.profiles=local` | health details, `com.carddemo` DEBUG, `carddemo.initial-load.on-startup=${CARDDEMO_INITIAL_LOAD:true}` |
| `test` | Surefire / Failsafe (`-Dcarddemo.test.profiles`, default `test`) | datasource from Testcontainers, quiet logs, `flyway.clean-disabled`, a random per-JVM JWT key from the test-only `RandomJwtSecretForTests` (nothing committed), `carddemo.batch.scheduler.enabled=false`, `carddemo.reports.async.enabled=false` |
| `ci` | GitHub Actions, layered on `test` (`-Dcarddemo.test.profiles=test,ci`) | no ANSI colours, Flyway/Testcontainers INFO logs |
| `golden` | golden-set / parity runs (`scripts/golden-set`, `scripts/batch`, `--spring.profiles.active=golden`) | frozen clock `2022-07-06T00:00:00` UTC and the pinned JCL parameters of the GnuCOBOL baseline (ADR-0014), cron off, synchronous reports |

## 2. `carddemo.*` properties

| Property | Default (`application.yml`) | Env var | Bound by | Meaning |
| --- | --- | --- | --- | --- |
| `carddemo.clock.fixed` | unset (system clock) | — | `common.time.ClockProperties` | freezes the injected business `Clock` (`COB_CURRENT_DATE`); `golden`: `2022-07-06T00:00:00` |
| `carddemo.clock.zone` | `UTC` | — | `ClockProperties` | zone of `FUNCTION CURRENT-DATE` / `ASKTIME` values and of the scheduler cron |
| `carddemo.security.jwt.secret` | none | `CARDDEMO_JWT_SECRET` | `web.security.JwtProperties` | HS256 key of the session token, ≥ 32 bytes (ADR-0017); no default in any profile, compose refuses to start without it (s6.4) |
| `carddemo.security.jwt.issuer` | `carddemo` | — | `JwtProperties` | `iss` claim written and required |
| `carddemo.security.jwt.ttl` | `PT1H` | `CARDDEMO_JWT_TTL` | `JwtProperties` | token lifetime (ISO-8601 duration) |
| `carddemo.online.applid` | `CARDDEMO` | `CARDDEMO_APPLID` | `web.OnlineProperties` | `EXEC CICS ASSIGN APPLID` in every screen header |
| `carddemo.online.sysid` | `CDMO` | `CARDDEMO_SYSID` | `OnlineProperties` | `EXEC CICS ASSIGN SYSID` |
| `carddemo.initial-load.on-startup` | `false` (`local`: `true`) | `CARDDEMO_INITIAL_LOAD` (`local` only) | `batch.load.InitialLoadProperties` | run `initial-load` when the app starts; a completed run for the same source files (SHA-256) is not repeated |
| `carddemo.initial-load.source-dir` | `app/data/EBCDIC` | `CARDDEMO_INITIAL_LOAD_SOURCE_DIR` | `InitialLoadProperties` | directory of the `AWS.M2.CARDDEMO.*.PS` images; a relative path is searched from the working directory upwards |
| `carddemo.initial-load.mode` | `replace` | `CARDDEMO_INITIAL_LOAD_MODE` | `InitialLoadProperties` | `replace` (truncate and load) or `upsert` |
| `carddemo.initial-load.max-rejects` | `0` | — | `InitialLoadProperties` | records per dataset that may fail to map before the step fails |
| `carddemo.batch.output-dir` | `batch-output` | `CARDDEMO_BATCH_OUTPUT_DIR` | `batch.BatchOutputProperties` | root of dated outputs and default output DDs (§4); compose mounts the `carddemo-batch-output` volume at `/app/batch-output` |
| `carddemo.batch.retain` | `5` | — | `BatchOutputProperties` | generations kept per GDG base (`LIMIT(5) SCRATCH`), must be ≥ 1 |
| `carddemo.batch.creastmt.html-escape` | `false` | — | `batch.creastmt.CreastmtJobConfiguration` (`@Value`) | `true` escapes customer names/addresses in STATEMNT.HTML (rules `CBSTM03A.md` D-1); `false` keeps the legacy bytes the golden set compares |
| `carddemo.batch.scheduler.enabled` | `true` (`test`/`golden`: `false`) | `CARDDEMO_SCHEDULER_ENABLED` | `batch.scheduler.NightlyCycleScheduling` (`@ConditionalOnProperty`) | registers the `nightly-cycle` cron trigger; web application only, never in a `--job=` CLI process |
| `carddemo.batch.scheduler.nightly-cycle.cron` | `0 0 22 * * *` | `CARDDEMO_NIGHTLY_CYCLE_CRON` | `NightlyCycleTrigger` (`@Scheduled`) | Spring 6-field cron (sec min hour dom mon dow) in `carddemo.clock.zone`; `run-date` = today on the business clock |
| `carddemo.reports.async.enabled` | `true` (`test`/`golden`: `false`) | — | `batch.report.ReportExecutorConfiguration` | CORPT00C report requests run on the single-thread `ReportExecutor`; off = on the calling thread (202 + COMPLETED) |
| `carddemo.reports.async.queue-capacity` | `20` | — | `ReportExecutorConfiguration` | waiting report requests; one more → 503 (ADR-0021). The queue is in-process: one app instance |
| `carddemo.reports.encoding` | `EBCDIC` | — | `TransactionReportLauncher` | code page of the TRANREPT bytes returned by `GET /api/v1/reports/transactions/{id}/report`; the golden set runs with `ASCII` |
| `carddemo.baseline.intcalc-parm-date` | unset (`golden`: `2022071800`) | — | `batch.BaselineRunProperties` | `PARM` of INTCALC STEP15 (CBACT04C); unset = run date |
| `carddemo.baseline.tranrept-start-date` / `-end-date` | unset (`golden`: `2022-01-01` / `2022-07-06`) | — | `BaselineRunProperties` | TRANREPT `DATEPARM` window; unset = business clock |
| `carddemo.baseline.waitstep-centiseconds` | unset (`golden`: `3600`) | — | `BaselineRunProperties` | recorded for parity; WAITSTEP is retired (06-scheduling.md §4) |

Spring's own settings that matter: `spring.datasource.url|username|password` ← `CARDDEMO_DB_URL` (default
`jdbc:postgresql://localhost:5432/carddemo`) / `CARDDEMO_DB_USER` (`carddemo`) / `CARDDEMO_DB_PASSWORD` (none);
`spring.batch.job.enabled=false` (must stay false: the CLI refuses it, RC 16); `spring.jpa.hibernate.ddl-auto=validate`
(Flyway owns the schema); `springdoc.api-docs.path=/v3/api-docs`, `springdoc.swagger-ui.path=/swagger-ui.html`.

## 3. Environment variables

| Variable | Used by | Default | Notes |
| --- | --- | --- | --- |
| `CARDDEMO_DB_PASSWORD` | app + compose Postgres | none (required) | never passed as a job parameter (the CLI rejects `--*password*`, ADR-0015) |
| `CARDDEMO_DB_URL` | app | `jdbc:postgresql://localhost:5432/carddemo` | compose sets `jdbc:postgresql://postgres:5432/<db>` |
| `CARDDEMO_DB_USER` / `CARDDEMO_DB_NAME` | app, compose | `carddemo` | |
| `CARDDEMO_DB_PORT` | compose | `5432` | host port of Postgres |
| `CARDDEMO_JWT_SECRET` | app, compose (required) | none | ≥ 32 bytes, e.g. `openssl rand -base64 48`; also keys the opaque `cardRef` (ADR-0020), so rotating it invalidates tokens and card references |
| `CARDDEMO_JWT_TTL` | app | `PT1H` | |
| `CARDDEMO_APPLID` / `CARDDEMO_SYSID` | app | `CARDDEMO` / `CDMO` | screen header region ids |
| `CARDDEMO_INITIAL_LOAD` | app (`local`), compose | `true` | |
| `CARDDEMO_INITIAL_LOAD_MODE` | app, compose | `replace` | |
| `CARDDEMO_INITIAL_LOAD_SOURCE_DIR` | app | `app/data/EBCDIC` | the image bakes the sample data in |
| `CARDDEMO_BATCH_OUTPUT_DIR` | app | `batch-output` | |
| `CARDDEMO_SCHEDULER_ENABLED` | app | `true` | |
| `CARDDEMO_NIGHTLY_CYCLE_CRON` | app | `0 0 22 * * *` | |
| `CARDDEMO_PROFILES` | compose → `SPRING_PROFILES_ACTIVE` | `local` | |
| `CARDDEMO_HTTP_PORT` | compose | `8080` | host port of the app; the UI dev server expects `8084` (`CARDDEMO_API_URL`) |
| `CARDDEMO_UI_PORT` | compose, Playwright | `8085` | host port of the nginx UI |
| `CARDDEMO_BIND_ADDRESS` | compose | `127.0.0.1` | host interface for every published port |
| `CARDDEMO_API_URL` | Vite dev server | `http://localhost:8084` | proxy target of `npm run dev` |
| `CARDDEMO_UI_URL` | Playwright e2e | `http://localhost:${CARDDEMO_UI_PORT:-8085}` | |
| `MAVEN_MIRROR_URL` | image build | Maven Central | |
| `CARDDEMO_JAR`, `GOLDEN_*`, `CARDDEMO_BASELINE_DIR`, `NIGHTLY_CYCLE_*` | `scripts/golden-set`, `scripts/batch` | see `modernization/README.md` | test tooling, not the app |

## 4. Batch: DD parameters, file vs table mode, outputs

A JCL `DD` becomes a command-line parameter with the DD name (ADR-0015):

- **Inputs.** `--<DD>=table` (default for KSDS datasets) reads the PostgreSQL table in key order (keyset browse,
  ADR-0011); `--<DD>=<path>` reads a file in the copybook layout. `--encoding=EBCDIC|ASCII` (default `EBCDIC`) is
  the code page of every file DD; `--record-prefix=ZOS_RDW|GNUCOBOL_VARSEQ_0|GNUCOBOL_VARSEQ|NONE` (default
  `ZOS_RDW`) frames RECFM=V files.
- **Updated KSDS** (POSTTRAN's ACCTFILE/TCATBALF, INTCALC's ACCTFILE, ...) follow the same rule: `table` updates the
  table, a path updates the file in place (file mode is what the equivalence scripts use).
- **Outputs.** Without a parameter an output DD goes to `<carddemo.batch.output-dir>/<DSN>`; GDG outputs are dated
  generations `<output-dir>/<base>/<base>.<business date>.<job execution id>` registered in `batch_output_file`
  (record count + SHA-256, ADR-0012). `(0)` / `(-1)` are resolved from that table; inside one stream a step reads the
  file the earlier step wrote (`--<DD>.GENERATION=<file>` binds it explicitly). After each new generation the
  oldest beyond `carddemo.batch.retain` (5) are deleted with their rows.
- **SYSOUT.** `--SYSOUT=<path>`; default `<output-dir>/SYSOUT/<job>.<run-date>.<execution id>.txt`.
- **Streams.** Step-qualified parameters `--<STEP>.<DD>=` reach one step of a multi-step stream
  (`--STEP15.DATEPARM=...`); in `nightly-cycle`, `--<MEMBER>.<name>=` reaches one member
  (`--POSTTRAN.STEP15.SYSOUT=...`).
- **Data movement.** `--job=unload --DATASET=<ds> --OUTFILE=<path>` dumps a table in key order in the copybook
  layout; `--job=repro --DATASET=<ds> --INFILE=<path> [--mode=replace|upsert]` loads one back.

## 5. Online API

- JWT (ADR-0017): `POST /api/v1/auth/login` returns a bearer token signed with `carddemo.security.jwt.secret`,
  valid `ttl`; there is no server-side session. A user demoted from admin keeps admin access until the token expires.
- Region ids in headers: `carddemo.online.applid` / `sysid`. Business date/time in headers: the injected clock.
- OpenAPI: `/v3/api-docs`, Swagger UI `/swagger-ui.html` (both proxied by the UI on `CARDDEMO_UI_PORT`).
