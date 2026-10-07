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
`batch_run` (one job row + one row per step: status, RC, read/write counts). Jobs so far: `initial-load`,
`cbexport`, `cbimport`, and the print jobs `READACCT` (CBACT01C), `READCARD` (CBACT02C), `READXREF` (CBACT03C),
`READCUST` (CBCUS01C) in `com.carddemo.batch.print`. Example (the GnuCOBOL baseline's inputs and output framing):

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

## Run locally (docker compose)

`docker-compose.yml` starts PostgreSQL 16, the application (one service: the modular monolith) and the web UI
(`carddemo-ui`, nginx serving the React build), all with health checks; the app waits for a healthy database and
Flyway migrates the schema on start, the UI waits for a healthy app.

```bash
cd modernization
cp .env.example .env          # set CARDDEMO_DB_PASSWORD; no password default is shipped
docker compose up -d --build --wait
curl http://localhost:8080/actuator/health      # {"status":"UP","components":{"db":{"status":"UP",...
# Swagger UI: http://localhost:8080/swagger-ui.html
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
