# CardDemo on Java 21

Java 21 rewrite of the CardDemo core application (`app/`: CICS online programs + JCL batch). It is one
Spring Boot application, a **modular monolith**: one domain package per business area, with dependencies
between packages enforced by ArchUnit at build time. See
[ADR-0001](../docs/modernization/adr/ADR-0001-modular-monolith.md) and the
[ADR index](../docs/modernization/adr/README.md) for the COBOL → Java mapping rules every later ticket follows.

## Build

**Java 21 is required, and the default JDK on the Devin machines is not 21.** `java` defaults to 11 and
plain `mvn` picks up another JDK (17, or 25 on some images), so always set `JAVA_HOME` explicitly:

```bash
cd modernization
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify
```

`maven-enforcer-plugin` (`requireJavaVersion [21,22)`, `requireMavenVersion 3.6.3+`) fails the build with
this hint if another JDK is used. `mvn verify` runs:

| Phase | What | Needs |
| --- | --- | --- |
| `test` (Surefire) | unit tests, ArchUnit module rules (+ self-test on broken fixtures), golden-profile clock test | JDK 21 |
| `integration-test` (Failsafe, `*IT`) | `CardDemoApplicationIT`: boots the app on PostgreSQL 16 via Testcontainers, checks Flyway, JPA, Spring Batch job repository, actuator health, OpenAPI | Docker |

## Layout

```
modernization/
├── pom.xml                      parent: spring-boot-starter-parent 3.3.x, Java 21, enforcer, BOMs
└── carddemo-app/                the single Spring Boot application
    └── src/main/java/com/carddemo/
        ├── CardDemoApplication.java
        ├── common/              shared kernel: AbendException, CICS RESP exceptions + HTTP mapping, business Clock
        ├── customer/            CUSTDAT                                  (may use: common)
        ├── user/                USRSEC, sign-on, menus, COUSR0*C         (may use: common)
        ├── card/                CARDDAT, CARDAIX, CCXREF, CXACAIX        (may use: common)
        ├── account/             ACCTDAT, COACTVWC, COACTUPC              (may use: common, card, customer)
        ├── transaction/         TRANSACT, TCATBALF, DISCGRP, TRANTYPE/CATG, COTRN0*C, COBIL00C
        │                                                                 (may use: common, account, card, customer)
        └── batch/               one Spring Batch job per JCL job         (may use: everything; nobody uses batch)
```

Each domain keeps its non-public types in a `<domain>.internal` sub-package, which other domains may not use.
The rules live in `src/test/java/com/carddemo/architecture/ModularMonolithRules.java`.

Stack: Spring Web, Validation, Data JPA, Spring Batch 5, Flyway (PostgreSQL), springdoc-openapi 2,
Actuator; tests with JUnit 5, Spring Boot Test, Spring Batch Test, Testcontainers, ArchUnit.

Flyway owns the whole schema, including the Spring Batch job repository (`V1__spring_batch_job_repository.sql`,
copied from `spring-batch-core`), so `spring.batch.jdbc.initialize-schema=never`. Jobs never launch at startup
(`spring.batch.job.enabled=false`); the in-app scheduler and REST trigger launch them.

## Batch CLI

`java -jar carddemo-app.jar --job=<name> [--run-date=YYYY-MM-DD] [--<DD>=<path>|table ...]` runs one job without
a web server and exits with its JCL condition code 0/4/8/12/16
([ADR-0015](../docs/modernization/adr/ADR-0015-batch-harness-return-codes.md)); every run is recorded in
`batch_run` (one job row + one row per step: status, RC, read/write counts). Jobs: `initial-load`, `cbexport`,
`cbimport`, `unload`/`repro`, the print jobs `READACCT` (CBACT01C), `READCARD` (CBACT02C), `READXREF` (CBACT03C),
`READCUST` (CBCUS01C), the streams `posttran`, `intcalc`, `tranbkp`, `combtran`, `tranrept`, `creastmt`,
`prtcatbl`, and `nightly-cycle` (JCL member → job: `docs/modernization/07-traceability.md` §4). DD parameters,
file vs table mode and output locations: `docs/modernization/09-configuration.md` §4. Example (the GnuCOBOL
baseline's inputs and output framing):

```bash
java -jar carddemo-app/target/carddemo-app.jar --spring.profiles.active=golden --job=READACCT \
  --ACCTFILE=../app/data/ASCII/acctdata.txt --encoding=ASCII --record-prefix=GNUCOBOL_VARSEQ_0 \
  --OUTFILE=/tmp/READACCT/OUTFILE --ARRYFILE=/tmp/READACCT/ARRYFILE --VBRCFILE=/tmp/READACCT/VBRCFILE \
  --SYSOUT=/tmp/READACCT/sysout.txt; echo "RC=$?"
```

Without `--ACCTFILE` the job reads the `account` table. `make batch-equivalence` runs the four print jobs from
files and from PostgreSQL and compares them with `docs/validation/baseline/<JOB>/`
(`scripts/batch/run_print_jobs.sh`, `scripts/batch/compare_print_jobs.py`).

`--job=nightly-cycle --run-date=YYYY-MM-DD` runs the whole nightly batch cycle (print jobs, POSTTRAN, INTCALC,
TRANBKP, COMBTRAN, TRANREPT, CREASTMT, PRTCATBL) as one Spring Batch flow job with the scheduler conditions as
`COND`, exit code = JCL MAXCC ([ADR-0016](../docs/modernization/adr/ADR-0016-nightly-cycle-flow-job.md),
`docs/modernization/06-scheduling.md`). The web app also fires it on the cron
`carddemo.batch.scheduler.nightly-cycle.cron` (default `0 0 22 * * *`) unless `CARDDEMO_SCHEDULER_ENABLED=false`;
the `test` and `golden` profiles disable the cron. `make nightly-cycle` runs it from freshly loaded sample data in
file and table mode and compares every output with the baseline.

Against the compose database, the same CLI runs inside the app container (it reuses the container's datasource
settings; the web app keeps running):

```bash
docker compose exec carddemo-app java -jar /app/carddemo-app.jar --job=readacct; echo "RC=$?"
docker compose exec carddemo-app java -jar /app/carddemo-app.jar --job=nightly-cycle --run-date=2022-07-06
```

Operating the cycle (order, bypass rules, RCs, `batch_run` queries, reruns after a failure, CycleLock):
[`docs/modernization/10-runbook-nightly-cycle.md`](../docs/modernization/10-runbook-nightly-cycle.md).

## Equivalence levels

| Level | Command | Proves | Needs |
| --- | --- | --- | --- |
| 1 | `make baseline-check` | the GnuCOBOL baseline in `docs/validation/baseline/` is reproducible | `cobc` 3.1.2 |
| 2 | `make batch-equivalence` | every Java batch job (and `nightly-cycle`) matches the baseline, file and table mode | PostgreSQL 16 via `CARDDEMO_DB_URL`/`CARDDEMO_DB_USER`/`CARDDEMO_DB_PASSWORD`, packaged jar |
| 3 | `make golden-set` | online scenario + whole cycle: Java == COBOL field by field; writes `docs/validation/golden-set/<date>/` | Docker, `cobc`, jar |
| 4 | `make golden-set-check` | level 3 plus no drift from the newest committed reconciliation (CI gate) | as level 3 |

`make traceability-check` (CI `build` job) keeps the COBOL → Java traceability matrix
(`docs/modernization/07-traceability.md`) in step with the sources and fails on any unmapped item.

## Golden set (end-to-end equivalence)

`make golden-set` (`scripts/golden-set/run_golden_set.sh`, about two minutes, needs Docker, GnuCOBOL `cobc` and the
packaged jar — it builds it when missing, `GOLDEN_BUILD=1` forces a rebuild) is the one end-to-end proof that
online changes followed by the whole nightly cycle give the same result in Java and in COBOL:

1. Java: throwaway `postgres:16-alpine`, `initial-load` from `app/data/EBCDIC`, the web app under the `golden`
   profile, the scripted REST scenario `online_scenario.sh` (inputs in `scenario.json`: account/customer update,
   card update, two transaction adds, a bill payment, user add/update/delete, a Custom report + download), then
   `--job=unload` of the six online datasets.
2. COBOL: `apply_online_scenario.py` applies the same scenario to the pristine sample records from the rules
   documents (copybook offsets, no Java code); `compare_datasets.py` compares the two field by field — the online
   equivalence proof. The report download must be byte-identical to the GnuCOBOL TRANREPT for the same window.
3. `cobol_cycle.py` runs the GnuCOBOL cycle on the after-online files (baseline machinery, same clock pins);
   `run_nightly_cycle.sh table` runs `--job=nightly-cycle` on the online-changed database and compares every job
   with that run (`CARDDEMO_BASELINE_DIR`); the final datasets are compared field by field.
4. `golden_report.py` writes `docs/validation/golden-set/<UTC date>/reconciliation.md` (+ `what-this-does-not-prove.md`)
   and exits non-zero on any unexplained difference or unused allow-list entry (`scripts/golden-set/expected-diffs/`,
   one entry per difference with its ADR/rules reference; key `*` = the same field in every record).

Knobs: `GOLDEN_OUT` (default `build/golden-set`), `GOLDEN_DATE`/`GOLDEN_DOC_DIR`, `GOLDEN_PG_PORT` (55433),
`GOLDEN_APP_PORT` (18095), `CARDDEMO_JAR`, `GOLDEN_JAVA_HOME`. Credentials are random per run and never written.

### Golden set in CI (`golden-set` job, gate g-golden)

The `golden-set` job of `.github/workflows/modernization-ci.yml` runs on every PR and push to `main` that touches the
workflow's path filters (including `scripts/golden-set/**` and `docs/validation/golden-set/**`). It installs Temurin
21 (Maven cache) and the `gnucobol` + `jq` apt packages, packages the jar with `mvn -DskipTests package` (the `build`
job runs the test suite; packaging here keeps the job parallel with the others instead of waiting for `build`), and
runs `make golden-set-check` with `CARDDEMO_JAR` pointing at it. Docker on the runner hosts the script's own
throwaway `postgres:16-alpine`, so the script runs unchanged. The GnuCOBOL compile takes seconds, so there is no
binary cache.

`make golden-set-check` (`scripts/golden-set/ci_check.sh`) is to the golden set what `make baseline-check` is to the
baseline. It runs `run_golden_set.sh` with `GOLDEN_DOC_DIR=build/golden-set-doc` and fails when:

- the run fails (any unexplained difference, any allow-list entry not matched exactly once, missing dataset,
  duplicate key, load/scenario failure), or
- the reconciliation it wrote differs from the newest committed `docs/validation/golden-set/<date>/`
  (`GOLDEN_COMMITTED_DIR` overrides). Two things are normalised because they vary by host: the `Toolchain:` line of
  `reconciliation.md` (JDK vendor/build, cobc patch level), and trailing fraction zeros on transcript lines that hold
  a bare JSON number (jq 1.6 pretty-prints the API's `1020.00` as `1020`, jq 1.7 keeps the literal; the runner's
  apt jq is 1.7.1, the committed transcript was written with 1.6). Every other table, count, digest and transcript
  line must be identical.

The job writes `build/golden-set/summary.md` (verdict, per-comparison lines, allow-list count, elapsed, plus any
unexplained-difference lines or drift diff) to the step summary. It uploads `golden-set-reconciliation` (the
`docs/validation/golden-set/<date>/` layout) and `golden-set-work` (datasets, compare reports, logs) as artifacts. When a
change is meant to alter the reconciliation (a new scenario step, a new allow-list entry), regenerate the dated
directory with `make golden-set` and commit it together with the change.

## Run locally (docker compose)

`docker-compose.yml` starts PostgreSQL 16, the application (one service: the modular monolith) and the web UI
(`carddemo-ui`, nginx serving the React build), all with health checks; the app waits for a healthy database and
Flyway migrates the schema on start, the UI waits for a healthy app.

```bash
cd modernization
cp .env.example .env          # set CARDDEMO_DB_PASSWORD; no password default is shipped
# If the image build fails with "status code: 429" from repo.maven.apache.org, add to .env:
#   MAVEN_MIRROR_URL=https://maven-central.storage-download.googleapis.com/maven2/
docker compose up -d --build --wait
curl http://localhost:8080/actuator/health      # {"status":"UP","components":{"db":{"status":"UP",...
# On start-up (local profile) the app runs initial-load of app/data/EBCDIC before it reports healthy.
# Swagger UI: http://localhost:8080/swagger-ui.html   (OpenAPI JSON: /v3/api-docs; also via the UI port)
# Web UI:     http://localhost:8085  (sign on as USER0001 or ADMIN001)
docker compose down           # add -v to drop the database volume
```

The image is built from the repository root (`Dockerfile`, context `..`), because the codec reads the legacy
copybooks from `app/cpy`. Variables (shell or `.env`):

| Variable | Default | Purpose |
| --- | --- | --- |
| `CARDDEMO_DB_PASSWORD` | none (required) | Postgres password for both containers |
| `CARDDEMO_JWT_SECRET` | none outside `local`/`test` | HS256 key (≥ 32 bytes) of the online API tokens (ADR-0017); `local` has a development-only fallback |
| `CARDDEMO_JWT_TTL` | `PT1H` | lifetime of a sign-on token |
| `CARDDEMO_HTTP_PORT` | `8080` | host port of the app; set it when 8080/8084 are taken |
| `CARDDEMO_DB_PORT` | `5432` | host port of Postgres |
| `CARDDEMO_UI_PORT` | `8085` | host port of the web UI (nginx: static React build, proxies `/api/`, `/v3/`, `/swagger-ui*` to the app) |
| `CARDDEMO_BIND_ADDRESS` | `127.0.0.1` | host interface all ports are published on (`local` shows health details) |
| `CARDDEMO_DB_NAME` / `CARDDEMO_DB_USER` | `carddemo` | database and user |
| `CARDDEMO_PROFILES` | `local` | `SPRING_PROFILES_ACTIVE` of the app container |
| `MAVEN_MIRROR_URL` | Central | Maven mirror for the image build (use when Central answers 429) |

Every other `CARDDEMO_*` variable and `carddemo.*` property (scheduler cron, batch output directory, report queue,
initial-load mode, ...) is in [`docs/modernization/09-configuration.md`](../docs/modernization/09-configuration.md).

From the repo root the same is `make up`, `make health`, `make down`.

Without Docker for the app (Postgres still needed):

```bash
export CARDDEMO_DB_PASSWORD=...   # CARDDEMO_DB_URL defaults to jdbc:postgresql://localhost:5432/carddemo
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B -pl carddemo-app spring-boot:run -Dspring-boot.run.profiles=local
```

## Web UI (`carddemo-ui/`)

React 18 + Vite + TypeScript, one route per BMS map (`docs/modernization/08-ui-map.md`, ADR-0022). It talks only to
`/api/v1`; routing follows `NavigationContext.toProgram`, the JWT and navigation context live in `sessionStorage`.

```bash
cd modernization/carddemo-ui
npm ci
npm run dev        # http://localhost:5173; proxies /api, /v3, /swagger-ui to CARDDEMO_API_URL (default http://localhost:8084)
npm run lint && npm run typecheck && npm test && npm run build
```

Run the API for the dev server with `CARDDEMO_HTTP_PORT=8084 docker compose up -d --wait carddemo-app` (or set
`CARDDEMO_API_URL=http://localhost:8080`). In compose the `carddemo-ui` image (multi-stage: Node 20 build → nginx)
serves the same build on `CARDDEMO_UI_PORT`.

End-to-end (Playwright, Chromium) against the running compose stack: signs in as USER0001 and ADMIN001 and completes
every transaction once on the sample data (account 00000000010):

```bash
docker compose up -d --build --wait          # from modernization/
cd carddemo-ui && npx playwright install chromium
npm run e2e                                  # CARDDEMO_UI_URL / CARDDEMO_UI_PORT select the UI (default :8085)
```

What the E2E suite and recordings do not cover is listed in `docs/modernization/08-ui-map.md` ("Not tested").

## Spring profiles

| Profile | Used by | Sets |
| --- | --- | --- |
| (none) | everything | `application.yml`: datasource from `CARDDEMO_DB_*`, Flyway, Batch (no start-up launch), actuator `health,info` |
| `local` | docker compose, `spring-boot:run` | health details, `com.carddemo` DEBUG logging |
| `test` | Surefire/Failsafe (system property from `carddemo.test.profiles`, default `test`) | health details, quiet logs, `flyway.clean-disabled`; datasource comes from Testcontainers |
| `ci` | GitHub Actions (`-Dcarddemo.test.profiles=test,ci`) | no ANSI colours, Flyway/Testcontainers INFO logs |
| `golden` | parity / equivalence runs | baseline clock and JCL parameters (below) |

## CI

`.github/workflows/modernization-ci.yml` runs on pull requests (any base branch, since board PRs are stacked)
and pushes to `main` that touch `modernization/**`, `app/cpy|cbl|data/**`, `scripts/baseline/**`,
`scripts/batch/**`, `docs/validation/baseline/**`, `app/bms/**`, `app/cpy-bms/**`, the `Makefile` or the workflow
(`modernization/carddemo-ui/**` is listed explicitly):

| Job | What |
| --- | --- |
| `build` | Temurin 21, `mvn -B verify -Dcarddemo.test.profiles=test,ci` (unit tests, ArchUnit, JaCoCo codec gate, Testcontainers Postgres ITs); uploads `jacoco-report` and `test-reports` artifacts |
| `compose` | `docker compose up -d --build --wait`, asserts `/actuator/health` is `UP` and the UI answers, then runs the Playwright USER/ADMIN flows (`npm run e2e`) against the stack; report as the `playwright-report` artifact |
| `ui` | Node 20 in `modernization/carddemo-ui`: `npm ci`, `npm run lint`, `npm run typecheck`, `vitest --run`, `vite build` |
| `baseline` | installs `gnucobol` (3.1.2), `make baseline-check`: compiles and runs the 26 batch jobs with `--fast`, asserts `jobs=26 compile failures=0`, and fails if any job output, report or gnucobol patch differs from `docs/validation/baseline/` (toolchain-specific `00-COMPILE/*.log`, `cobc-*.txt` are reported, not gated) |
| `batch-equivalence` | PostgreSQL 16 service + packaged jar: `make batch-equivalence` runs READACCT/READCARD/READXREF/READCUST through the batch CLI (file input, then table input after `initial-load`), compares SYSOUT (trailing spaces normalised), datasets (byte-level) and exit codes with `docs/validation/baseline/<JOB>/`, and checks that an abend exits 16 with a `batch_run` row; the same for POSTTRAN, INTCALC, TRANBKP/COMBTRAN/TRANREPT/PRTCATBL and CREASTMT, then `make nightly-cycle` (one `--job=nightly-cycle` launch per mode, chained Java outputs, job × mode × result matrix); report in the job summary, outputs as the `batch-equivalence` artifact |

Locally: `make verify`, `make baseline-check` (needs `cobc` 3.1.2: `sudo apt-get install gnucobol`).

## Reproducing the COBOL baseline (golden profile)

The GnuCOBOL baseline (`scripts/baseline/`, outputs in `docs/validation/baseline/`) runs with a frozen clock and
pinned JCL parameters. The `golden` Spring profile pins the same values
([ADR-0014](../docs/modernization/adr/ADR-0014-baseline-clock-pin.md)):

| Baseline | Java (`application-golden.yml`) |
| --- | --- |
| `COB_CURRENT_DATE=2022/07/06 00:00:00.00` | `carddemo.clock.fixed=2022-07-06T00:00:00` (injectable `java.time.Clock`) |
| `INTCALC` `PARM='2022071800'` | `carddemo.baseline.intcalc-parm-date=2022071800` |
| `TRANREPT` / `DATEPARM` `2022-01-01`..`2022-07-06` | `carddemo.baseline.tranrept-start-date` / `-end-date` |
| `WAITSTEP` `SYSIN 00003600` | `carddemo.baseline.waitstep-centiseconds=3600` |

Code must take dates and times from the injected `Clock`, never from `LocalDate.now()` without one.
