# Dependency scan (s6.4)

Run with `make dependency-scan` (`scripts/hardening/dependency_scan.sh`): `npm audit --omit=dev` for the UI, then the
Snyk CLI for the Java reactor when it is installed and authenticated, else OWASP `dependency-check-maven`
(`NVD_API_KEY` strongly recommended). Not a CI gate: both feeds are network services and flaky in CI.

Scanned 2026-10-07 on `devin/unt51-26-hardening`.

## Tools and what happened

| Scope | Tool | Result |
| --- | --- | --- |
| `modernization/carddemo-ui` production deps | `npm audit --omit=dev` | **found 0 vulnerabilities** |
| `modernization/` (Maven reactor, runtime + test) | Snyk CLI `snyk test --all-projects` | before: 112 open (12 critical, 50 high, 44 medium, 6 low); after the upgrades below: **23 open (2 critical, 11 high, 9 medium, 1 low)**, all triaged below |
| `modernization/` | OWASP `dependency-check-maven` | started; the NVD 2.0 feed (≈ 402,800 CVE records) downloads at ≈ 10,000 records per 5 min without an API key, so the run did not finish in the session. Snyk was used instead, as the ticket allows. With `NVD_API_KEY` set, `DEPENDENCY_SCAN_TOOL=owasp make dependency-scan` runs it (`-DfailBuildOnCVSS=7`). |

## Upgrades made (all in `modernization/pom.xml`)

| Component | Before | After | Why |
| --- | --- | --- | --- |
| Spring Boot parent | 3.3.13 (OSS support ended) | **3.5.16** | Framework 6.1 → 6.2.19, Security 6.3 → 6.5.11, Batch 5.1 → 5.2.6, Data JPA 3.5.13, Micrometer 1.15.12, Flyway 11.7.2; 112 → 47 open findings |
| springdoc-openapi | 2.6.x | 2.8.17 | Boot 3.5 compatible line |
| Tomcat (`tomcat.version`) | 10.1.55 (Boot 3.5.16) | 10.1.60 | 1 critical (FORM auth bypass), 1 critical (WebSocket alternate name), 4 high, 4 medium |
| Jackson BOM (`jackson-bom.version`) | 2.21.4 | 2.21.7 | 4 high, 6 medium (databind/core) |
| Logback (`logback.version`) | 1.5.34 | 1.5.38 | 1 high (Janino `<if>` expression injection) |
| PostgreSQL JDBC (`postgresql.version`) | 42.7.11 | 42.7.13 | 1 high (SCRAM channel binding fail-open) |
| commons-lang3 (`commons-lang3.version`) | 3.17.0 | 3.18.0 | uncontrolled recursion fix (CVE-2025-48924); 47 → 23 open with the four overrides above |

Effect on behaviour: none observed — the full `mvn -B verify` (1127 unit + 131 ITs) and `make golden-set-check`
pass on the upgraded set locally, and all six `modernization-ci` jobs run on it for the PR head. Remove each property override
once the Boot BOM manages an equal or newer version.

## Remaining findings (Snyk, after the upgrades)

The fixes for everything below exist only in Spring Framework 7.0 / Security 7.0 / Batch 6.0 (Spring Boot 4) or
Micrometer 1.16 (Boot 4's line); a Boot 4 migration (Jakarta EE 11, Jackson 3, Hibernate 7) is out of scope for
the hardening step and is listed in `docs/modernization/11-handover.md` next steps. Each finding was checked against
what the application actually uses (`grep` of `modernization/carddemo-app/src/main` and the dependency tree).

### Critical / high

| Sev | Package | CVE | Issue | Fixed in | Applies to CardDemo? |
| --- | --- | --- | --- | --- | --- |
| critical | spring-webmvc 6.2.19 | CVE-2026-47884 | Code injection via `XsltView` with a `/**` mapping and implicit view name | 7.0.9 | **No.** REST controllers only (`@RestController`, JSON bodies); no view resolvers, no XSLT views, no `/**` view mapping. |
| critical | spring-security-oauth2-jose 6.5.11 | CVE-2026-41707 | DPoP proof replay via `DPoPProofJwtDecoderFactory` cache eviction | 7.0.7 | **No.** Bearer HS256 tokens via `NimbusJwtDecoder.withSecretKey` (`web.security.SecurityConfiguration`); DPoP is not configured. |
| high | spring-beans 6.2.19 | CVE-2026-59282 | Memory exhaustion via data-binding property path with a large index into a self-populating list | 7.0.9 | **No.** No `@ModelAttribute`/`WebDataBinder` binding onto objects with auto-growing lists; request bodies are Jackson-bound records, query parameters are scalar `@RequestParam`s. |
| high | spring-web 6.2.19 | CVE-2026-59281 | XSS when rendering `Errors.getFieldErrors()` in server-side views | 7.0.9 | **No.** No server-side HTML rendering; errors are JSON `ApiError` bodies, the React UI renders text nodes. |
| high | spring-web 6.2.19 | CVE-2026-47885 | Multipart memory exhaustion in `PartEventHttpMessageReader` with `maxInMemorySize=-1` | 7.0.9 | **No.** WebFlux reader, not used; no multipart endpoints. |
| high | spring-web 6.2.19 | CVE-2026-47889 | Missing `SameSite` on cookies in `JettyCoreServerHttpResponse` | 7.0.9 | **No.** Tomcat, not Jetty; the API sets no cookies (stateless bearer tokens). |
| high | spring-expression 6.2.19 | CVE-2026-47886 | CPU/memory exhaustion evaluating untrusted SpEL with `^` on BigDecimal | 7.0.9 | **No.** No user-supplied SpEL is evaluated. |
| high | spring-expression 6.2.19 | CVE-2026-59283 | `SimpleEvaluationContext` + SpEL compiler class-loading growth | 7.0.9 | **No.** `spring.expression.compiler.mode` not set; no untrusted SpEL. |
| high | spring-security-core 6.5.11 | CVE-2026-59276 | Non-constant-time compare in `DigestAuthenticationFilter`, `KeyBasedPersistenceTokenService`, Password4j encoders, `InMemoryOAuth2AuthorizationService` | 7.0.7 | **No.** None of those classes is used; passwords use BCrypt (`DelegatingPasswordEncoder`, ADR-0023), JWT signatures are verified by Nimbus. |
| high | spring-security-crypto 6.5.11 | CVE-2026-47842 | Predictable IV in `AesBytesEncryptor` CBC mode | 7.0.7 | **No.** `web.card.CardReferences` uses JCA AES-GCM with a random 96-bit IV, not `AesBytesEncryptor`. |
| high | spring-batch-infrastructure 5.2.6 | CVE-2026-47881 | `FlatFileItemReader` multi-line record CPU/memory exhaustion | 6.0.5 | **No.** Batch inputs are read by `com.carddemo.common.file.RecordFiles` (fixed-length/line-sequential), not `FlatFileItemReader`; inputs are operator-supplied datasets. |
| high | micrometer-core 1.15.12 | CVE-2026-59295 | Memory leak in `MicrometerHttpClientInterceptor` | 1.16.7 | **No.** Apache HttpClient instrumentation not used. |
| high | micrometer-core 1.15.12 | CVE-2026-59296 | CRLF injection in StatsD line builders and `LoggingMeterRegistry` | 1.16.7 | **No.** Only the actuator's in-memory registry; no StatsD or logging registry, meter names are not user input. |

### Medium / low (accepted, same reasoning)

| Sev | Package | CVE | Issue | Note |
| --- | --- | --- | --- | --- |
| medium | spring-web 6.2.19 | CVE-2026-59314 | HTTP response splitting | Fix 7.0.9; no user input in response headers except `Location` built from generated ids. |
| medium | spring-webmvc 6.2.19 | CVE-2026-47890 | XSS | Fix 7.0.9; no server-side views. |
| medium | spring-webmvc 6.2.19 | CVE-2026-47887, CVE-2026-47883 | Open redirect | Fix 7.0.9; no redirect views/`redirect:` returns. |
| low | spring-webmvc 6.2.19 | CVE-2026-59313 | Improper neutralization | Fix 7.0.9; same scope. |
| medium | spring-batch-core 5.2.6 | CVE-2026-47875, CVE-2026-47878 | Deserialization of untrusted data | Fix 6.0.5; execution contexts are read back only from the app's own PostgreSQL job repository, not from an untrusted source. |
| medium | spring-data-jpa 3.5.13 | CVE-2026-47834 | SQL injection | Fix 4.0.7; all queries are derived or parameterised `@Query`; no user-controlled sort/property names. |
| medium | logback-classic 1.5.38 | CVE-2026-19880 | Directory traversal | Fix 1.6.3 (Boot 4 line); logback config is shipped in the jar, not user-controlled. |
| medium | log4j-api 2.24.3 | CVE-2026-49844 | Improper output encoding | No fix released; `log4j-to-slf4j` bridge only, Log4j core is not on the classpath. |

**Open high/critical without justification: none.**
