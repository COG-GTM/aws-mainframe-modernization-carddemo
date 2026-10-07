# Clean-checkout walkthrough of `modernization/README.md`

Acceptance test for the operations README (UNT51-25 / plan step s6.3): a fresh clone of
`devin/unt51-25-traceability-docs` at `c9fa97b` in a new directory (`~/walk/carddemo`, not the working tree),
following `modernization/README.md` literally. Run on 2026-10-07 (UTC) on the Devin Ubuntu 22.04 machine.

Toolchain found on the machine: default `java` 11, `mvn` 3 on JDK 25 (hence the `JAVA_HOME` rule), JDK 21 at
`/usr/lib/jvm/java-21-openjdk-amd64`, Docker Compose v5.4.0, GnuCOBOL `cobc` 3.1.2.0, `jq` 1.6, Node 20.20.2 /
npm 10.8.2. Ports 8080, 8085 and 5432 were free; 8084 was taken by an unrelated process (the README's
`CARDDEMO_HTTP_PORT` override covers that case; not needed here).

## 1. Clone

```bash
git clone -b devin/unt51-25-traceability-docs https://github.com/COG-GTM/aws-mainframe-modernization-carddemo.git carddemo
cd carddemo && git log --oneline -1
```

Result: `c9fa97b feature: COBOL→Java traceability matrix (make traceability/-check, CI build step) + configuration,
runbook and README operations docs`.

## 2. Build (README "Build")

```bash
cd modernization
JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify
```

Result: `BUILD SUCCESS` in 1 min 20 s (local Maven repository already warm). `maven-enforcer-plugin`
`RequireJavaVersion` and `RequireMavenVersion` passed on both modules. Surefire `Tests run: 1091, Failures: 0,
Errors: 0, Skipped: 0`; Failsafe (Testcontainers PostgreSQL 16) `Tests run: 122, Failures: 0, Errors: 0, Skipped: 0`.

## 3. Run with docker compose (README "Run locally")

```bash
cp .env.example .env          # CARDDEMO_DB_PASSWORD set to a random value (not recorded)
docker compose up -d --build --wait
```

Result, first attempt: **failed** after 1 min 17 s. The image build (`mvn -B -ntp -DskipTests package` inside
`maven:3.9-eclipse-temurin-21`) could not resolve `spring-boot-starter-parent:pom:3.3.13`: `status code: 429,
reason phrase: Too Many Requests` from `repo.maven.apache.org`. The README listed `MAVEN_MIRROR_URL` only in the
variable table, so a reader following the commands had no hint. **README fix:** the compose block now says what to
add to `.env` on a 429.

```bash
echo "MAVEN_MIRROR_URL=https://maven-central.storage-download.googleapis.com/maven2/" >> .env
docker compose up -d --build --wait
```

Result: exit 0 in 2 min 0 s; `carddemo-postgres-1`, `carddemo-carddemo-app-1` and `carddemo-carddemo-ui-1` all
`Healthy` (the app reports healthy only after the start-up `initial-load` of `app/data/EBCDIC`).

```bash
curl http://localhost:8080/actuator/health
```

Result: `{"status":"UP","components":{"db":{"status":"UP","details":{"database":"PostgreSQL",...`.

## 4. Sign on (curl, through the UI's nginx on 8085)

```bash
curl -s -X POST http://localhost:8085/api/v1/auth/login -H 'Content-Type: application/json' \
     -d '{"userId":"USER0001","password":"PASSWORD"}'
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:8085/
```

Result (token elided): `{"token":"<jwt>","tokenType":"Bearer","expiresAt":"…","userId":"USER0001","role":"USER",
"userType":"U","targetMenu":"COMEN01C","targetMenuUrl":"/api/v1/menu/main","navigation":{"fromTranId":"CC00",
"fromProgram":"COSGN00C","toTranId":"CM00","toProgram":"COMEN01C","pgmContext":"ENTER",...}}`; the UI root answers
`200`. The browser flow sign-on → main menu → account view was recorded separately by the testing agent (linked on
the ticket and the PR).

## 5. One batch job from the CLI (README "Batch CLI")

```bash
docker compose exec carddemo-app java -jar /app/carddemo-app.jar --job=readacct; echo "RC=$?"
```

Result: `batch CLI: launching readacct with run.id=…,run-date=2026-10-07`, `Executing step: [STEP05]`,
`readacct ended COMPLETED RC=0000`, `RC=0`.

## 6. Golden set (README "Golden set")

```bash
cd ..        # repository root
make golden-set
```

Result: exit 0 in 1 min 23 s, `golden-set: PASS` — initial-load rows `10/7/18/51/50/50/50/50/50/0/300`; online
scenario 30 REST steps, 10 records changed; online changes PASS (0 unexplained); report download byte-identical
to the COBOL TRANREPT; nightly cycle PASS (cycle RC 4, all 11 members ok); final datasets PASS (0 unexplained);
allow-list 11 entries, all matched exactly once. It rewrote `docs/validation/golden-set/2026-10-07/` and
`git status` stayed clean afterwards: the regenerated reconciliation is identical to the committed one.

## 7. Web UI checks (README "Web UI")

```bash
cd modernization/carddemo-ui
npm ci
npm run lint && npm run typecheck && npm test && npm run build
```

Result: exit 0 in 12 s — ESLint clean, `tsc --noEmit` clean, Vitest `Test Files 6 passed (6)`, `Tests 44 passed (44)`
(includes the BMS field check), Vite build `✓ built in 764ms`.

## README corrections prompted by this walkthrough

| Where | Problem | Fix |
| --- | --- | --- |
| `modernization/README.md`, "Run locally" | `docker compose up --build` fails on a Maven Central 429 with no hint in the command block | comment with the `MAVEN_MIRROR_URL` line to add to `.env` |

Everything else ran as written.
