# ADR-0001: One Spring Boot application, domain packages enforced by ArchUnit

- Status: Accepted (UNT51-5, 2026-10-07)
- Applies to: `modernization/carddemo-app`

## Context
The plan decision `d-decomp` picked a modular monolith over a service per domain. The earlier
`devin/1789660764-carddemo-java-modernization` branch ran auth/customer/account/card/transaction services on
ports 8081-8085. That split does not fit CardDemo: `COACTUPC` rewrites ACCTDAT and CUSTDAT in one unit of work,
and the batch chain (POSTTRAN → INTCALC → TRANREPT/CREASTMT) reads every file. The AWS track's `aws/services` and
`aws/batch` (see `00-prior-work.md`) were single applications too.

## Decision
- One Maven reactor `modernization/`, one module `carddemo-app`, one Spring Boot application `com.carddemo.CardDemoApplication`.
- Packages: `common` (shared kernel), `customer`, `user`, `card`, `account`, `transaction`, `batch`.
- Allowed dependencies (besides `common`, JDK, frameworks):

| Package | May use |
| --- | --- |
| `common` | no domain |
| `customer`, `user`, `card` | - |
| `account` | `card`, `customer` |
| `transaction` | `account`, `card`, `customer` |
| `batch` | every domain |

- No domain depends on `batch`; package cycles are forbidden; types in `<domain>.internal` are private to that domain.
- Only `batch` uses the Spring Batch API. Online programs become REST controllers + services in their domain.
- Flyway owns the whole schema, including the Spring Batch job repository tables.

## Enforcement
`src/test/java/com/carddemo/architecture/ModularMonolithRules.java`, run by `ArchitectureTest` on production classes
and by `ModularMonolithRulesSelfTest` on deliberately broken fixtures (`archfixture.*`) so the rules are proven to fail.

## Consequences
One deployable and one transaction manager; cross-domain updates (COACTUPC) stay ACID. Splitting a domain out
later means turning its public package API into a remote API; the dependency matrix shows where the cuts are.
