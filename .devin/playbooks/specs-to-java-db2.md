# Specs -> Java microservice on Db2 (verify + gap review)

Takes a folder of business-logic specs (AWS Transform output, or the same layout written by hand) for one legacy COBOL/Db2 module. Produces a tested Java 21 / Spring Boot 3 microservice running on a real IBM Db2 in Docker, a gap review of the legacy code against the spec and the Java, a recorded UI test, and one PR.

Reference run: `specs/aws-transform/transaction-type/` -> `modernization/transaction-type-service/` (CardDemo transaction-type module).

## Inputs (ask only if they can't be inferred)
- `LEGACY_DIR`: legacy module folder, e.g. `app/app-transaction-type-db2/` or `app/app-authorization-ims-db2-mq/`
- `TARGET_TABLES`: the Db2 tables in scope (DDL under `LEGACY_DIR/ddl`), e.g. `AUTHFRDS`
- `PROGRAMS`: the COBOL programs that own those tables, e.g. `COPAUS2C.cbl`
- `SPECS_DIR`: `specs/aws-transform/<module>/`. If it doesn't exist, phase 1 creates it.
- `SERVICE_DIR`: `modernization/<module>-service/`
- Repo conventions: every commit message contains "feature" or "bug"; the last line of the PR body is `Devin-Org: engineering`.

## Phase 1 - Specs folder
1. If `SPECS_DIR` has AWS Transform output (download it from the AWS Transform job: business-logic extraction / technical documentation), commit it unchanged and note the job ID in `SPECS_DIR/README.md`.
2. Otherwise write the specs yourself in the AWS Transform business-logic layout: one file per program (overview, inputs/outputs, flow, PF keys / entry points, SQL), `DATA-MODEL.md` (DDL, DCLGEN host variables, keys, FKs, seed data), and `BUSINESS-RULES.md` (catalog with stable IDs `BR-<MOD>-nnn`, each with its COBOL paragraph). Every hand-written file starts with a provenance header saying it was authored by Devin in AWS Transform's format, **not** by AWS Transform.
3. Only scope what `PROGRAMS` touch on `TARGET_TABLES`. For IMS/MQ programs, model the IMS segments and MQ messages as inputs/outputs; do not implement IMS or MQ unless the specs require it.

**Checkpoint 1:** every rule has an ID and a COBOL source reference. `grep -c "BR-" BUSINESS-RULES.md` is > 0.

## Phase 2 - Implement from the specs
1. Create `SERVICE_DIR` with Maven, Java 21 (`JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64`; plain `mvn` may pick an older JDK), Spring Boot 3, `spring-boot-starter-web`, `-jdbc`, `-validation`, `-actuator`, `springdoc-openapi-starter-webmvc-ui`, and `com.ibm.db2:jcc`.
2. Layout: `domain/` (records, rules class, service, store port), `adapters/db2/` (JdbcTemplate store that reproduces the COBOL cursors and SQL), `api/` (controller + exception handler that maps rule failures and SQLCODEs to HTTP statuses).
3. Put the rule ID in a comment next to the code that implements it. Use the legacy message text from the COBOL literals, word for word.
4. Add unit tests (in-memory store fake that can inject SQLCODEs) and MockMvc tests. Every test's `@DisplayName` starts with its rule ID.

**Checkpoint 2:** `mvn -B verify` is green. Every catalog ID appears in both `src/main` and `src/test`:
`comm -23 <(grep -oE "BR-[A-Z]+-[0-9]{3}" SPECS_DIR/BUSINESS-RULES.md | sort -u) <(grep -rhoE "BR-[A-Z]+-[0-9]{3}" SERVICE_DIR/src/test | sort -u)` prints nothing.

## Phase 3 - Real Db2 in Docker
1. Add `SERVICE_DIR/docker-compose.yml` with `icr.io/db2_community/db2:latest`, `privileged: true`, `LICENSE=accept`, `DBNAME`, `DB2INST1_PASSWORD`, port 50000, mount `LEGACY_DIR` read-only and an init script into `/var/custom`.
2. The init script applies `LEGACY_DIR/ddl/*.ddl` and the load cards. Make it idempotent (skip if the tables exist). Strip z/OS-only clauses (`ERASE NO`, `CLOSE NO`, `STOGROUP`, `BUFFERPOOL`, `IN DATABASE`), and tolerate SQL0605W (duplicate index). If the init script fails, the container still reports "Setup has completed", so health-check on a marker file the script touches.
3. Add a `Makefile` with `db2-up`, `db2-wait`, `db2-sql Q=...`, `run`, `up`, `it`, and a `Dockerfile` that copies a jar built on the host (Maven inside Docker builds may not reach the network).
4. First start pulls ~1.5 GB and takes a few minutes. Wait for the healthcheck, not a fixed sleep.

**Checkpoint 3:** `make db2-sql Q="SELECT COUNT(*) FROM <schema>.<table>"` returns the seed row counts from the load cards.

## Phase 4 - Tests on real Db2
1. Add a `db2-it` Maven profile (Failsafe, `*IT.java`). Run `@SpringBootTest(webEnvironment=RANDOM_PORT)` against the container and cover seeded reads, paging, create/update/delete persistence, and the real SQLCODEs (-803 duplicate, -532 FK restrict, +100 not found).
2. Run `mvn -B -Pdb2-it verify`. Keep the output and quote only the numbers you saw.
3. Start the service (`make run` or `make up`). After any source change, rebuild it with `make up`: a container can be healthy and still run an old jar, so a passing health check does not prove it runs the current code. Check `/actuator/health` is UP and `/swagger-ui/index.html` returns 200.

**Checkpoint 4:** unit + IT green; health UP; Swagger 200.

## Phase 5 - Open the PR, then UI test with recording
1. Push the branch and open one PR (draft is fine). The PR body includes the specs provenance, the API table, the test numbers, and the AWS resources to tear down (if any).
2. Hand off to the testing agent. It starts Db2 + the service itself, opens Swagger UI in the browser, and records: list + paging (next/previous), filters, get, create, update, delete with and without confirmation, a validation error, a SQLCODE error (duplicate / FK restrict), and a `db2` CLI query in the terminal proving each write landed.
3. Attach the recording and key screenshots to the PR in a comment.

**Checkpoint 5:** recording shows each flow, and the Db2 CLI output matches the API responses.

## Phase 6 - Legacy vs spec vs Java gap review
1. Read every COBOL paragraph of `PROGRAMS` next to the specs and the Java. Grep all COBOL string literals and diff them against the Java messages.
2. Write `SPECS_DIR/GAP-REVIEW.md` with tables for SQLCODE handling, paging / PF keys / cursors, validation, confirmation flows, batch record actions, and extracts. Mark each behaviour: captured in spec? implemented + tested in Java? Use **MISSED** / **FIXED** / *intentional deviation* / not applicable (with a reason).
3. For each miss: add or correct the rule in the spec, fix the code, add a test, then re-run Checkpoints 2 and 4.
4. In the PR description, add a "What the spec missed" section that lists the misses.

**Checkpoint 6:** no unexplained rows in GAP-REVIEW.md. Tests green after the fixes.

## Phase 7 - Wrap up
Update the PR description with a service README traceability table (rule -> code -> test). Report the PR link, the recording, the test numbers and any blockers. Do not merge.
