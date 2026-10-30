# Specs -> Java microservice on Db2 (verify + gap review)

Takes a folder of business-logic specs (AWS Transform output, or the same layout written by hand) for one legacy COBOL/Db2 module. Produces a tested Java 21 / Spring Boot 3 microservice running on a real IBM Db2 in Docker, a gap review of the legacy code against the spec and the Java, a recorded UI test, and one PR.

Reference runs: `specs/aws-transform/transaction-type/` -> `modernization/transaction-type-service/` (CardDemo transaction-type, a CRUD maintenance table), and `specs/aws-transform/authfrds/` -> `modernization/authfrds-service/` (AUTHFRDS / COPAUS2C, a write-only fraud table reached from an IMS/CICS program).

## Inputs (ask only if they can't be inferred)
- `LEGACY_DIR`: legacy module folder, e.g. `app/app-transaction-type-db2/` or `app/app-authorization-ims-db2-mq/`
- `TARGET_TABLES`: the Db2 tables in scope (DDL under `LEGACY_DIR/ddl`), e.g. `AUTHFRDS`
- `PROGRAMS`: the COBOL programs that own those tables, e.g. `COPAUS2C.cbl`
- `SPECS_DIR`: `specs/aws-transform/<module>/`. If it doesn't exist, phase 1 creates it.
- `SERVICE_DIR`: `modernization/<module>-service/`
- Repo conventions: every commit message contains "feature" or "bug"; the last line of the PR body is `Devin-Org: engineering`.

## Phase 1 - Specs folder
1. If `SPECS_DIR` has AWS Transform output (download it from the AWS Transform job: business-logic extraction / technical documentation), commit it unchanged and note the job ID in `SPECS_DIR/README.md`. Job outputs are in the job's **Artifacts** tab under `results/`; a browser download works (`page.expect_download` over CDP). Steps can stay at "Awaiting user input" with no approvals listed even after you confirm in the chat. Time-box the job, commit whatever partial output exists (e.g. only Analyze code) under `SPECS_DIR/aws-transform-output/`, say which steps finished, and continue with step 2.
2. Otherwise write the specs yourself in the AWS Transform business-logic layout: one file per program (overview, inputs/outputs, flow, PF keys / entry points, SQL), `DATA-MODEL.md` (DDL, DCLGEN host variables, keys, FKs, seed data), and `BUSINESS-RULES.md` (catalog with stable IDs `BR-<MOD>-nnn`, each with its COBOL paragraph). Every hand-written file starts with a provenance header saying it was authored by Devin in AWS Transform's format, **not** by AWS Transform.
3. Only scope what `PROGRAMS` touch on `TARGET_TABLES`. For IMS/MQ programs, model the IMS segments and MQ messages as inputs/outputs; do not implement IMS or MQ unless the specs require it.
4. Record the program's real verbs (e.g. COPAUS2C only does INSERT, and UPDATE on -803; it never deletes). Later phases test those verbs, not a generic CRUD set.

**Checkpoint 1:** every rule has an ID and a COBOL source reference. `grep -c "BR-" BUSINESS-RULES.md` is > 0.

## Phase 2 - Implement from the specs
1. Create `SERVICE_DIR` with Maven, Java 21 (`JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64`; plain `mvn` may pick an older JDK), Spring Boot 3, `spring-boot-starter-web`, `-jdbc`, `-validation`, `-actuator`, `springdoc-openapi-starter-webmvc-ui`, and `com.ibm.db2:jcc`.
2. Layout: `domain/` (records, rules class, service, store port), `adapters/db2/` (JdbcTemplate store that reproduces the COBOL cursors and SQL), `api/` (controller + exception handler that maps rule failures and SQLCODEs to HTTP statuses). Expose only the legacy verbs; a read-only inspection list is fine when the legacy has none (mark it Java-only in the catalog). If a verb doesn't exist (e.g. delete), don't invent it; mark it not applicable.
   - IMS/MQ: put each behind a port in `domain/` (e.g. `PendingAuthorizationStore`) with an in-memory stub in `adapters/`, seeded from the module's unload data if there is any. Give the stub a failure-injection hook (e.g. make the next REPL return a DL/I status), so the legacy SYNCPOINT ROLLBACK path can be tested. Add a test proving that a failure in the stubbed store rolls back the real Db2 write, and that a failed Db2 commit restores the stub. Serialise read-modify-write flows per key.
3. Put the rule ID in a comment next to the code that implements it. Use the legacy message text from the COBOL literals, word for word.
4. Add unit tests (in-memory store fake that can inject SQLCODEs) and MockMvc tests. Every test's `@DisplayName` starts with its rule ID.
5. Security (a Java-only rule in the catalog; the legacy relied on CICS signon/RACF):
   - Add `spring-boot-starter-security` and a `config/SecurityConfig` with stateless HTTP Basic and CSRF off.
   - Access: `GET /api/**` needs READER or MAINTAINER, other methods need MAINTAINER, and `/error`, `/actuator/health`, `/swagger-ui/**` and `/v3/api-docs/**` are public. Without `/error`, a real server turns every 404/400/500 into 403, because it error-dispatches there; MockMvc does not, so also test with `RANDOM_PORT` + `TestRestTemplate`. Bind to `127.0.0.1` by default (Basic auth over plain HTTP); the container sets `SERVER_ADDRESS=0.0.0.0` behind a loopback port mapping. A global springdoc `@SecurityScheme(name="basicAuth", type=HTTP, scheme="basic")` makes Swagger's Authorize work.
   - Credentials are env-only (`<SVC>_API_USER` / `_PASSWORD` for the maintainer, `<SVC>_API_READER_USER` / `_PASSWORD` for the reader), bound to `carddemo.security.{maintainer,reader}.{username,password}`, with no committed defaults. Startup fails fast if any is blank or the two usernames match. The unit-test `test` profile is exempt; its creds live in `src/test/resources/application-test.yml`, and surefire sets `spring.profiles.active=test`.
   - Test or debug endpoints go in their own controller with `@Profile("testing")`, off by default.
   - Tests: 401 with no or a wrong password, 403 for a reader write, 200 for reader GET and maintainer write, health and api-docs public, test endpoints 404 without the `testing` profile, and a context-runner test that startup fails when creds are blank.

**Checkpoint 2:** `mvn -B verify` is green, including the security tests. Every catalog ID appears in both `src/main` and `src/test`:
`comm -23 <(grep -oE "BR-[A-Z]+-[0-9]{3}" SPECS_DIR/BUSINESS-RULES.md | sort -u) <(grep -rhoE "BR-[A-Z]+-[0-9]{3}" SERVICE_DIR/src/test | sort -u)` prints nothing.

## Phase 3 - Real Db2 in Docker
1. Add `SERVICE_DIR/docker-compose.yml` with `icr.io/db2_community/db2:latest`, `privileged: true`, `LICENSE=accept`, `DBNAME`, `DB2INST1_PASSWORD`, port 50000, mount `LEGACY_DIR` read-only and an init script into `/var/custom`. Secrets come only from an uncommitted `SERVICE_DIR/.env`: commit `.env.example` with placeholders, gitignore `.env`, use `${DB2_PASSWORD:?set in .env}` in compose, and `spring.datasource.password: ${SPRING_DATASOURCE_PASSWORD}` with no default.
2. The init script applies `LEGACY_DIR/ddl/*.ddl` and the load cards. Make it idempotent per stage: check each table and each seed load separately. Run every `db2` command in the shell that ran `db2 connect` (CLP connections belong to that shell, so `$(db2 ...)` runs without the connection); write results to a file and read them from there. Test the first boot on a **fresh** volume. Strip z/OS-only clauses (`ERASE NO`, `CLOSE NO`, `STOGROUP`, `BUFFERPOOL`, `IN DATABASE`), and tolerate SQL0605W (duplicate index) only, by grepping the CLP output, not with `|| true`. Db2 LUW may not raise it at all (e.g. a PK index ASC/ASC next to a legacy index ASC/DESC). If the init script fails, the container still reports "Setup has completed", so health-check on a marker file the script touches.
3. Add a `Makefile` (`-include .env` + `export`, and a `check-env` guard; pass secrets through the exported environment, e.g. failsafe `${env.DB2_PASSWORD}` and `"$$DB2_PASSWORD"`, never by interpolating `$(DB2_PASSWORD)` into a command line) with `db2-up`, `db2-wait`, `db2-sql Q=...`, `run`, `up`, `it`, and a `Dockerfile` that copies a jar built on the host (Maven inside Docker builds may not reach the network).
4. First start pulls ~1.5 GB and takes a few minutes. Wait for the healthcheck, not a fixed sleep.

**Checkpoint 3:** `make db2-sql Q="SELECT COUNT(*) FROM <schema>.<table>"` returns the seed row counts from the load cards. If the module ships no Db2 load cards (e.g. AUTHFRDS: `LOADPADB.JCL` loads IMS only), the expected count is 0. Say so in `DATA-MODEL.md`, and have every IT create and clean up its own rows.

## Phase 4 - Tests on real Db2
1. Add a `db2-it` Maven profile (Failsafe, `*IT.java`). Run `@SpringBootTest(webEnvironment=RANDOM_PORT)` against the container and cover seeded reads, paging, persistence of each legacy verb, and the real SQLCODEs the programs handle (-803 duplicate, -532 FK restrict if there are FKs, +100 not found). ITs run with profiles `test,testing` and `TestRestTemplate.withBasicAuth`.
2. Run `mvn -B -Pdb2-it verify`. Keep the output and quote only the numbers you saw.
3. Start the service (`make run` or `make up`). After any source change, rebuild it with `make up`: a container can be healthy and still run an old jar, so a passing health check does not prove it runs the current code. Check `/actuator/health` is UP and `/swagger-ui/index.html` returns 200.

**Checkpoint 4:** unit + IT green; health UP; Swagger 200.

## Phase 5 - Open the PR, then UI test with recording
1. Push the branch and open one PR (draft is fine). The PR body includes the specs provenance, the API table, the test numbers, and the AWS resources to tear down (if any). Generate the API table from `/v3/api-docs` (paths, methods, parameters), not from memory.
2. Hand off to the testing agent. It starts Db2 + the service itself (from `.env`, with `PROFILES=testing` if it needs the stub hooks), opens Swagger UI in the browser, and records: an unauthenticated 401, Authorize as reader (GET 200, write 403), then Authorize as maintainer. As maintainer it records list + paging (next/previous), filters, get, every legacy write verb (create, update, delete with and without confirmation where they exist), a validation error, a real SQLCODE error, any IMS/MQ-stub rollback, and a `db2` CLI query in the terminal proving each write landed (or didn't, for rollbacks).
3. Attach the recording and key screenshots to the PR in a comment.

**Checkpoint 5:** recording shows each flow, and the Db2 CLI output matches the API responses.

## Phase 6 - Legacy vs spec vs Java gap review
1. Read every COBOL paragraph of `PROGRAMS` next to the specs and the Java. Grep all COBOL string literals and diff them against the Java messages. Check Db2 LUW vs z/OS differences with real queries: date/time formats (e.g. `TIMESTAMP_FORMAT('YY-...')` resolves YY to 20YY on LUW), and +100 on a searched UPDATE/DELETE (JCC returns update count 0, not SQLCODE +100, so the adapter must map it).
2. Write `SPECS_DIR/GAP-REVIEW.md` with tables for SQLCODE handling, paging / PF keys / cursors, validation, confirmation flows, batch record actions, extracts, and access control / test hooks. Mark each behaviour: captured in spec? implemented + tested in Java? Use **MISSED** / **FIXED** / *intentional deviation* / not applicable (with a reason).
3. For each miss: add or correct the rule in the spec, fix the code, add a test, then re-run Checkpoints 2 and 4.
4. In the PR description, add a "What the spec missed" section that lists the misses.

**Checkpoint 6:** no unexplained rows in GAP-REVIEW.md. Tests green after the fixes.

## Phase 7 - Wrap up
Update the PR description with a service README traceability table (rule -> code -> test). Report the PR link, the recording, the test numbers and any blockers. Do not merge.
