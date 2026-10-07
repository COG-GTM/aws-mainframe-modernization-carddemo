# Coverage (s6.4)

`mvn -B verify` in `modernization/` (JDK 21) on 2026-10-07: **1127 unit tests + 131 Testcontainers ITs, 0 failures.**

## Rules in `modernization/carddemo-app/pom.xml`

| Execution | Data | Scope | Rule |
| --- | --- | --- | --- |
| `check-codec-coverage` (UNT51-6) | `jacoco.exec` (unit) | `com.carddemo.common.{codec,date,file}` | ≥ 90 % LINE per class |
| `check-domain-coverage` (s6.4) | `jacoco-merged.exec` = unit (`jacoco.exec`) + IT (`jacoco-it.exec`, `prepare-agent-integration`), merged in `post-integration-test` | `com/carddemo/{account,card,transaction,user,customer,batch}/**` | ≥ `carddemo.domain.coverage.minimum` (0.80) LINE **per package** |

Both run in `verify`, so the CI `build` job fails below the floor. ITs are counted because the table-mode batch paths
(`KeyedDataset.table`, JPA writers, `TransactionIdStepLock`) only run against PostgreSQL. Reports:
`target/site/jacoco/` (unit) and `target/site/jacoco-merged/` (unit + IT).

The rule fails as intended: with the floor raised to 0.90 the same data fails the build with 10 packages listed.

```text
$ mvn -B -pl carddemo-app jacoco:check@check-domain-coverage -Dcarddemo.domain.coverage.minimum=0.90
[WARNING] Rule violated for package com.carddemo.batch.posttran: lines covered ratio is 0.89, but expected minimum is 0.90
[WARNING] Rule violated for package com.carddemo.batch.intcalc: lines covered ratio is 0.82, but expected minimum is 0.90
...
[ERROR] Failed to execute goal org.jacoco:jacoco-maven-plugin:0.8.12:check (check-domain-coverage) ... Coverage checks have not been met.
```

## Numbers (LINE, unit + IT merged)

Whole bundle: **92.8 %** (6930 / 7470 lines; unit only 91.1 %).

| Package | Unit only | Unit + IT | Lines covered / total |
| --- | --- | --- | --- |
| `com.carddemo.account` | 98.4 % | 98.4 % | 61 / 62 |
| `com.carddemo.account.online` | 99.4 % | 99.4 % | 352 / 354 |
| `com.carddemo.batch` | 89.4 % | 89.4 % | 76 / 85 |
| `com.carddemo.batch.creastmt` | 95.6 % | 95.6 % | 388 / 406 |
| `com.carddemo.batch.exchange` | 94.2 % | 94.2 % | 163 / 173 |
| `com.carddemo.batch.harness` | 85.2 % | 85.2 % | 655 / 769 |
| `com.carddemo.batch.housekeeping` | 88.3 % | 88.3 % | 250 / 283 |
| `com.carddemo.batch.intcalc` | 82.4 % | 82.4 % | 215 / 261 |
| `com.carddemo.batch.load` | 83.9 % | 83.9 % | 276 / 329 |
| `com.carddemo.batch.posttran` | 54.5 % | **89.9 %** | 312 / 347 |
| `com.carddemo.batch.print` | 89.5 % | 89.5 % | 213 / 238 |
| `com.carddemo.batch.report` | 90.2 % | 90.2 % | 238 / 264 |
| `com.carddemo.batch.scheduler` | 86.9 % | 86.9 % | 193 / 222 |
| `com.carddemo.batch.tranrept` | 87.7 % | 87.7 % | 250 / 285 |
| `com.carddemo.card` | 98.3 % | 98.3 % | 59 / 60 |
| `com.carddemo.card.online` | 97.6 % | 97.6 % | 206 / 211 |
| `com.carddemo.customer` | 98.7 % | 98.7 % | 75 / 76 |
| `com.carddemo.transaction` | 85.6 % | 85.6 % | 119 / 139 |
| `com.carddemo.transaction.online` | 95.1 % | 95.1 % | 271 / 285 |
| `com.carddemo.user` | 98.3 % | 98.3 % | 57 / 58 |
| `com.carddemo.user.admin` | 95.6 % | 95.6 % | 153 / 160 |
| `com.carddemo.user.menu` | 97.6 % | 97.6 % | 81 / 83 |
| `com.carddemo.user.signon` | 100.0 % | 100.0 % | 41 / 41 |

Lowest package: `batch.intcalc` at 82.4 % — the 46 uncovered lines are the I/O-error abend branches of `Cbact04c`
(`sysout.ioAbend(...)` after a `FileStatusException` on read/rewrite, which the sample data never triggers), the
`--PARM`/`--run-date` argument errors and the read-only guard lambdas of `IntcalcJobConfiguration`, and the
`XrefByAccount` file-mode status branches.
No getter-only tests were added for the floor; the s6.4 tests added are behaviour tests (password hash sign-on,
current-admin denial, PAN log capture, JWT secret startup, CREASTMT escaping, online-vs-POSTTRAN id race).

Not covered by these numbers: the React UI (Vitest + Playwright in the CI `ui` and `compose` jobs) and the
`com.carddemo.web` / `common` layers (outside the requested domain packages; `common.codec` has its own 90 % rule).
