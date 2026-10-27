# CardDemo transaction processing (Java)

A Spring Boot 3 / Spring Batch / Spring Data JPA port of the CardDemo transaction programs:

| COBOL | Java |
|---|---|
| `CBTRN01C` (batch: verify daily transactions) | `DailyTransactionValidationJob` (`batch/validation`) |
| `CBTRN02C` (batch: post daily transactions, JCL `POSTTRAN`) | `TransactionPostingJob` (`batch/posting`) |
| `COTRN00C` (CICS CT00: transaction list) | `GET /api/transactions` |
| `COTRN01C` (CICS CT01: view transaction) | `GET /api/transactions/{tranId}` |
| `COTRN02C` (CICS CT02: add transaction) | `POST /api/transactions` |

The online programs are included because they use the same TRANSACT / CARDXREF data as the batch jobs.
In Java, both sides share one JPA persistence layer (`domain` + `repository`).

## Build, test, run

Requires Java 21 and Maven 3.9.

```bash
cd app/app-java
mvn verify                                   # 41 JUnit tests using app/data/ASCII

# Batch: one job per run. The JVM exit code is the JCL return code (0, 4 = rejects, 12 = abend)
java -jar target/carddemo-transaction-0.1.0-SNAPSHOT.jar \
  --spring.batch.job.enabled=true --spring.batch.job.name=DailyTransactionValidationJob
java -jar target/carddemo-transaction-0.1.0-SNAPSHOT.jar \
  --spring.batch.job.enabled=true --spring.batch.job.name=TransactionPostingJob [dalytran=/path/file]

# Online REST API on :8080
java -jar target/carddemo-transaction-0.1.0-SNAPSHOT.jar
```

On first start, `SeedDataLoader` (the IDCAMS REPRO equivalent) loads ACCTDATA, CARDDATA, CARDXREF,
CUSTDATA and TCATBALF from `app/data/ASCII` into an empty database.

### Configuration (`application.yml`)

| Property / env var | Replaces | Default |
|---|---|---|
| `spring.datasource.url` / `CARDDEMO_DB_URL` | VSAM KSDS clusters | `jdbc:h2:file:./.h2/carddemo` |
| `carddemo.batch.dalytran-file` / `DALYTRAN` (or job parameter `dalytran`) | DD `DALYTRAN` | `../data/ASCII/dailytran.txt` |
| `carddemo.batch.dalyrejs-dir` / `DALYREJS` | DD `DALYREJS` (GDG +1) | `./batch-output` (`DALYREJS.GnnnnV00.txt` per run) |
| `carddemo.batch.chunk-size` | commit interval | 100 |
| `carddemo.seed.enabled`, `carddemo.seed.data-dir` | IDCAMS REPRO | `true`, `../data/ASCII` |

## Data mapping

| Copybook / FD | Record | Java | Table / key |
|---|---|---|---|
| `CVTRA06Y` | `DALYTRAN-RECORD` (350) | `domain.DailyTransaction` (record, not persisted) | sequential file |
| `CVTRA05Y` | `TRAN-RECORD` (350) | `domain.Transaction` | `TRANSACT` / `TRAN_ID` |
| `CVACT01Y` | `ACCOUNT-RECORD` (300) | `domain.Account` | `ACCTDATA` / `ACCT_ID` |
| `CVACT02Y` | `CARD-RECORD` (150) | `domain.Card` | `CARDDATA` / `CARD_NUM` |
| `CVACT03Y` | `CARD-XREF-RECORD` (50) | `domain.CardXref` | `CARDXREF` / `XREF_CARD_NUM`, index `CXACAIX` on `XREF_ACCT_ID` |
| `CVCUS01Y` | `CUSTOMER-RECORD` (500) | `domain.Customer` | `CUSTDATA` / `CUST_ID` |
| `CVTRA01Y` / `FD-TRAN-CAT-BAL-RECORD` | `TRAN-CAT-BAL-RECORD` (50) | `domain.TransactionCategoryBalance` | `TCATBALF` / (`acct-id`, `type-cd`, `cat-cd`) |

How COBOL semantics are kept (`cobol` package):

* **PIC X(n)**: every setter goes through `Pic.x`, so values are always exactly n characters, padded with spaces and truncated on the right.
* **PIC S9(i)V99** (DISPLAY in the extracts, COMP-3 on the mainframe): stored as `BigDecimal` with scale 2. `Pic.s9` truncates like a COBOL MOVE (no rounding, high-order digits dropped). `ZonedDecimal` decodes overpunched signs (`{`, `A`..`I`, `}`, `J`..`R`). `PackedDecimal` encodes and decodes COMP-3.
* **Fixed-width input**: `FieldCursor` / `RecordLayouts` read each field in copybook order using only the PIC sizes, so offsets are never hand-computed. Non-numeric data in a numeric field throws `CobolDataException` (the S0C7 equivalent).
* **INVALID KEY**: repository `findById` returns `Optional.empty()`.
* **FILE STATUS**: `FileStatus` enum. `00` means a record was returned; `10` means the reader returns `null`, which ends the step. Any other status throws `CobolFileException(ddName, status)`, which fails the step and the job (the abend).

## Paragraph mapping

### CBTRN01C → `DailyTransactionValidationJob`

| Paragraph | Java |
|---|---|
| `MAIN-PARA` loop | chunk step `CBTRN01C`: `DailyTransactionFileReader` → `DailyTransactionValidationProcessor.process` → `ValidationSummaryWriter` (counts per outcome in the step ExecutionContext) |
| `0000-DALYTRAN-OPEN` .. `0500-TRANFILE-OPEN` | `DailyTransactionFileReader.doOpen` (status 35 → abend); JPA DataSource for the KSDS files |
| `1000-DALYTRAN-GET-NEXT` | `DailyTransactionFileReader.doRead` (00 → record, 10 → `null`, other → `CobolFileException`) |
| `2000-LOOKUP-XREF` | `DailyTransactionValidationProcessor.lookupXref` → `XrefAccountLookup.readXref`; logs `CARD NUMBER x COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-y` |
| `3000-READ-ACCOUNT` | `DailyTransactionValidationProcessor.readAccount` → `XrefAccountLookup.readAccount`; logs `ACCOUNT x NOT FOUND` |
| `9000-..9500-*-CLOSE` | `ItemStream.close` / Spring context shutdown |
| `Z-DISPLAY-IO-STATUS` | `FileStatus.displayIoStatus` (included in the `CobolFileException` message) |
| `Z-ABEND-PROGRAM` | throw `CobolFileException`, giving job status `FAILED` and exit code 12 |

### CBTRN02C → `TransactionPostingJob`

| Paragraph | Java (`TransactionPostingService` unless noted) |
|---|---|
| main loop, counters, `RETURN-CODE 4` | chunk step `CBTRN02C`; `TransactionPostingJobConfig.PostingCounters` logs `TRANSACTIONS PROCESSED :` / `REJECTED  :` and sets exit status `COMPLETED WITH REJECTS`; `JclReturnCodes` maps the job result to exit code 0, 4 or 12 |
| `0000..0500-*-OPEN`, `9000..9500-*-CLOSE` | reader / `dalyrejsWriter` open and close |
| `1000-DALYTRAN-GET-NEXT` | `DailyTransactionFileReader.doRead` |
| `1500-VALIDATE-TRAN` | `validateTran` |
| `1500-A-LOOKUP-XREF` | `validateTran` → `XrefAccountLookup.readXref` (reason 100) |
| `1500-B-LOOKUP-ACCT` | `lookupAcct` (101 not found, 102 overlimit, 103 after expiration; when both 102 and 103 apply, 103 wins as in COBOL) |
| `2000-POST-TRANSACTION` | `postTransaction` |
| `Z-GET-DB2-FORMAT-TIMESTAMP` | `io.Db2Timestamp.now(clock)` gives `YYYY-MM-DD-HH.MM.SS.hh0000` |
| `2500-WRITE-REJECT-REC` | `writeRejectRec` + `dalyrejsWriter` (`RejectRecord`: 350-byte input + 80-byte trailer `9(04)` reason + `X(76)` description) |
| `2700-UPDATE-TCATBAL` / `2700-A-CREATE-TCATBAL-REC` / `2700-B-UPDATE-TCATBAL-REC` | `updateTcatbal` (upsert on `TransactionCategoryBalanceId`) |
| `2800-UPDATE-ACCOUNT-REC` | `updateAccountRec` (curr-bal += amt; amt >= 0 → cyc-credit, else cyc-debit; REWRITE INVALID KEY → reason 109, logged only, as in COBOL) |
| `2900-WRITE-TRANSACTION-FILE` | `writeTransactionFile` (duplicate TRAN-ID → status 22 → abend) |
| `9910-DISPLAY-IO-STATUS` / `9999-ABEND-PROGRAM` | `CobolFileException` (via `VsamIo` for persistence errors, status 30) |

### COTRN00C / COTRN01C / COTRN02C → REST

| Paragraph | Java |
|---|---|
| COTRN00C `PROCESS-ENTER-KEY` + `PROCESS-PAGE-FORWARD` | `GET /api/transactions?tranId=` → `TransactionInquiryService.firstPage` (10 rows, `nextPage` flag) |
| COTRN00C `PROCESS-PF8-KEY` | `GET /api/transactions?after=<last id>` → `nextPage` |
| COTRN00C `PROCESS-PF7-KEY` / `PROCESS-PAGE-BACKWARD` | `GET /api/transactions?before=<first id>` → `previousPage` |
| COTRN00C `POPULATE-TRAN-DATA` | `TransactionDtos.TransactionRow.of` (date `MM/DD/YY`, description X(26), amount `+99999999.99`) |
| COTRN01C `PROCESS-ENTER-KEY` / `READ-TRANSACT-FILE` | `GET /api/transactions/{id}` → `TransactionInquiryService.view` |
| COTRN02C `PROCESS-ENTER-KEY` | `POST /api/transactions` → `TransactionAddService.processEnterKey` (confirm `Y`/`N` handling) |
| COTRN02C `VALIDATE-INPUT-KEY-FIELDS`, `READ-CXACAIX-FILE`, `READ-CCXREF-FILE` | `validateInputKeyFields` |
| COTRN02C `VALIDATE-INPUT-DATA-FIELDS` (+ `CSUTLDTC`) | `validateInputDataFields` (strict `uuuu-MM-dd` check) |
| COTRN02C `ADD-TRANSACTION`, `WRITE-TRANSACT-FILE` | `addTransaction` (last TRAN-ID + 1; duplicate → 409 `Tran ID already exist...`) |
| COTRN02C `COPY-LAST-TRAN-DATA` (PF5) | `GET /api/transactions/last` returns the source record |

Error messages are copied word for word from the COBOL programs and returned as `{"message": ...}` with
an HTTP status (400 validation, 404 not found, 409 duplicate, 500 I/O).

## Deliberate differences / notes for review

* **Transactions.** VSAM writes are not transactional, so an abend in CBTRN02C leaves the earlier updates in place. In Java, each chunk commits atomically, so an abend rolls back the current chunk. Spring Batch restart then resumes from the last committed chunk.
* **CBTRN01C after EOF.** The COBOL loop runs `2000-LOOKUP-XREF` once more for the last record after EOF. This is a legacy quirk and is not reproduced.
* **REST input fields** are truncated to the COTRN2A map widths. For numeric fields (account, card, type, category, merchant id), trailing spaces are ignored before the `IS NUMERIC` test. On a 3270 screen those fields would be padded with spaces or low-values.
* **Sign-on / commarea navigation** (PF3 / XCTL, `CDEMO-*` context, menu return) is replaced by stateless REST calls. The paging state (`firstTranId`/`lastTranId`) is returned to the client.
* **Not ported:** EBCDIC/binary extracts can be decoded with `PackedDecimal`, but the seed loader reads only the ASCII files. There is no OpenAPI spec yet.
