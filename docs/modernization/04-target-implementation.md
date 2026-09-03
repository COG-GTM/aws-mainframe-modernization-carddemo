# 4. Target implementation — `modernization/posttran-cycle/`

Java 21, Spring Boot 3.3.4, Spring Batch 5, Maven multi-module. H2 in-memory holds only the
Spring Batch job repository; application data stays in fixed-width files so the services can be
dropped into the existing dataset flow (and so the byte-level tests in §5 are possible).

```
modernization/posttran-cycle/
├── pom.xml                          parent: versions, dependencyManagement, modules
├── carddemo-recordio/               shared: codec + copybook layouts + keyed store
├── transaction-posting-service/     POSTTRAN  → dailyTransactionPostingJob
├── interest-calculation-service/    INTCALC   → interestCalculationJob
└── transaction-report-service/      TRANREPT  → dailyTransactionReportJob (SORT + CBTRN03C)
```

Build and test:

```bash
export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
cd modernization/posttran-cycle && mvn test
```

## 4.1 Domain boundaries → services

| Boundary (from inventory) | Service | Owns | Reads only |
|---|---|---|---|
| Posting: DALYTRAN → TRANSACT, DALYREJS; mutates ACCTDATA, TCATBALF | `transaction-posting-service` | transaction master (this run), rejects, account cycle buckets, category balances | CARDXREF |
| Interest: TCATBALF → SYSTRAN; mutates ACCTDATA | `interest-calculation-service` | system transactions, account balance/cycle reset | TCATBALF, CARDXREF (AIX), DISCGRP |
| Reporting: TRANSACT → TRANREPT | `transaction-report-service` | report, intermediate sorted file | CARDXREF, TRANTYPE, TRANCATG, DATEPARM |

The two mutators share ACCTDATA. In the COBOL that is serialized by the scheduler; here it is
serialized the same way (separate jobs, no shared process). Moving ACCTDATA behind a single
account service is decision D-3/D-8 territory and is not done.

## 4.2 Idiomatic choices vs literal translation, and what each preserves

| COBOL | Java | Preserved behaviour |
|---|---|---|
| Six `OPEN` paragraphs, `READ … INTO`, `9910-DISPLAY-IO-STATUS`, `CEE3ABD` | `FixedWidthFile` / `KeyedRecordStore` throw `RecordFormatException`, `RecordNotFoundException`, `DuplicateKeyException`; a Spring Batch step that throws fails the job (`FAILED`), which is the U0999 equivalent | Any I/O condition the COBOL abends on fails the job; messages carry the COBOL text (`ERROR READING ACCOUNT FILE`, key, status 23) for log-diff. |
| `WS-VALIDATION-FAIL-REASON` numeric flag set by four sequential `IF`s | `TransactionValidator.validate(...)` returns `Optional<RejectReason>`; the enum carries code + 76-char text | Same order, same overwrite (103 beats 102), same equality semantics. The *reason* is data, not control flow, so it cannot be lost by a later paragraph. |
| `COMPUTE WS-TEMP-BAL` into `S9(09)V99` | `CobolNumeric.truncate(exposure, 9, 2)` before compare | Deliberately reproduces the high-order truncation (OQ-03) instead of "fixing" it; the test names it so a reviewer can decide. |
| `2700-UPDATE-TCATBAL` create-or-rewrite by status 23 | `PostingLedger.addToCategoryBalance` on `Optional` from the store | New row on miss, `REWRITE` on hit, other errors propagate. |
| `REWRITE FD-ACCTFILE-REC … INVALID KEY MOVE 109` (ignored) | `PostingLedger.applyToAccount` logs a WARN and keeps posting | Preserves the COBOL outcome (transaction still written, account update lost) and surfaces it in the log instead of silently. OQ-06. |
| 350-byte `TRAN-RECORD` / 430-byte `REJECT-RECORD` built by MOVEs | `TransactionLayout.encodeInto`, `RejectRecordLayout.encode(dailyImage, reason)` | Byte-exact lengths; reject copies the **original input image**, not a re-encoded one, so malformed-but-readable bytes survive. |
| `RETURN-CODE 4` when rejects > 0 | Step returns `ExitStatus("COMPLETED_WITH_REJECTS")`; `ExitCodeMapper` maps it to process exit 4 | Same scheduler contract. Not verified against a real launcher (see §5.3). |
| Control break on `TRANCAT-ACCT-ID` with `WS-FIRST-TIME` | `InterestPostingRun.accept()` / `finish()` — a stateful aggregator | Same break points; `finish()` on empty input is a no-op instead of rewriting uninitialised storage (OQ-12, documented deviation). |
| `READ XREF-FILE KEY IS FD-XREF-ACCT-ID` | `firstCardOf(accountId)` = min card number among the account's rows | Reproduces the AIX ordering assumption explicitly (`aix_card_xref_acct_id` in DDL). |
| `IF DIS-INT-RATE NOT = 0 PERFORM 1300…` | `if (rate.signum() != 0)` | Zero-rate rows produce no transaction and no interest. |
| DFSORT `INCLUDE COND` + `SORT FIELDS` | `TransactionSelector.selectAndSort` (stream filter + stable sort by card) as a separate tasklet step writing `TRANSACT.DALY` | Inclusive window, ascending card. Tie order is stable in Java, unspecified in DFSORT (OQ-14). |
| `NEXT SENTENCE` exit on out-of-range record | `OutOfRangePolicy.STOP_LIKE_COBOL` (default) / `SKIP_RECORD` | Default reproduces the source; the alternative exists because the intent is an open question (OQ-17), not because the code was "fixed". |
| EOF branch re-adds `TRAN-AMT` | `TransactionReportWriter.finish()` re-adds the last amount | Reproduced and tested (`singleRecordRunCountsTheLastAmountTwice…`), flagged OQ-18. |
| `PIC -ZZZ,ZZZ,ZZZ.ZZ` / `+ZZZ,ZZZ,ZZZ.ZZ` | `CobolEditedAmount.minusEdited/plusEdited` | 15 chars, floating sign, zero → spaces, truncation not rounding. |
| `FUNCTION CURRENT-DATE` → `DB2-FORMAT-TS` | `DateTimeFormatter("yyyy-MM-dd-HH.mm.ss.SSS'000'")` via an injectable `Clock` | 26-char layout with millisecond precision padded with `0000`, and tests can pin the clock. |
| Paragraph names | Class/method names carry the COBOL name in Javadoc (`/** 1500-B-LOOKUP-ACCT */`) rather than as identifiers | Traceability without exposing `1500B` as an API. |

## 4.3 Fixed-width / EBCDIC layer (`carddemo-recordio`)

* `FixedWidthRecord` wraps `byte[] + RecordEncoding` (IBM037 or US-ASCII). Field access is by
  offset/length; numeric access decodes zoned decimal with trailing overpunch sign (`{…I` / `}…R`).
* Each copybook has a `record` (immutable values) and a `Layout` (`decode`, `encodeInto`).
  `encodeInto` writes *only the named fields* into an existing image, which is how COBOL `MOVE`
  + `REWRITE` behaves: filler and unread bytes survive.
* `KeyedRecordStore` simulates a KSDS (sorted map, `read/write/rewrite`, status-23/22 as
  exceptions). It is a transition seam, not the target persistence — §3 DDL is.
* Reuse note: PR #6 in this repository contains an earlier fixed-width/EBCDIC layer. Its
  overpunch table and "raw image preserved through rewrite" idea were read and re-implemented
  here; no code was copied.

## 4.4 Known gaps (also in open-questions.md)

* Process exit code 4 relies on Spring Boot's `ExitCodeMapper` wiring, exercised only via
  `JobExecution.exitStatus` in tests.
* `KeyedRecordStore` loads whole files into memory; fine for the sample volumes, not for a
  production ACCTDATA.
* No restart/checkpoint semantics beyond what Spring Batch's job repository gives (the COBOL has
  none either, OQ-07).
