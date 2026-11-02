# COBOL-to-Java type and persistence conventions

These are the rules every port in `modernization/` follows when it turns a CardDemo copybook field,
file access or file status into Java, JSON and PostgreSQL. They replace the implicit conventions
listed in [architecture.md §2](architecture.md) and take precedence over
`modernization/docs/COPYBOOK-TO-SCHEMA-MAPPING.md` where the two differ (that file keeps the
per-table column lists).

Statements about COBOL behavior cite the source under `app/` (read-only). Anything inferred is
marked **Hypothesis**. The helpers named below live in
`modernization/common/src/main/java/com/carddemo/common/cobol/CobolValues.java` and are covered by
`CobolValuesTest`.

## 1. Summary

| COBOL | Java | PostgreSQL | JSON | Rule |
|---|---|---|---|---|
| `PIC S9(n)V99` (money) | `BigDecimal`, scale 2 | `NUMERIC(n+2,2)` | number | Results stored into a 2-decimal field are **truncated** (`CobolValues.truncate` / `CobolValues.divide`, `RoundingMode.DOWN`) unless the COBOL statement says `ROUNDED`. Never `double`. API inputs carry `@Digits(integer = n, fraction = 2)`. |
| `PIC 9(n)` id (account, customer, merchant) | `Long`, requests `@Min(0) @Max(10^n-1)` | `BIGINT` + `CHECK (x BETWEEN 0 AND 10^n-1)` | **string, zero-padded to n** (`@PicId(digits = n)`) | Leading zeros are a display property, never lost: `CobolValues.zeroPad`. |
| `PIC 9(n)` code (category, FICO) | `Integer` | `INTEGER` / `SMALLINT` | number | Not an id, no padding. |
| `PIC X(n)` | `String` | `VARCHAR(n)` (keys too) | string | Trimmed on load (`CobolValues.trim`, all blanks → `null`), right-padded on fixed-width output (`CobolValues.padRight`). |
| `PIC X(10)` date | `LocalDate` | `DATE` | `YYYY-MM-DD` | Only where the COBOL compares or validates the field as a calendar date (§4). Otherwise it stays `String`. |
| `PIC X(26)` timestamp | `LocalDateTime` | `TIMESTAMP` (µs) | ISO-8601 | `CobolValues.timestamp` / `toDb2Timestamp`. |
| `COMP-3` | `BigDecimal` | as the picture | – | `CobolValues.decodePacked`. |
| `COMP` / `BINARY` | `BigDecimal` / `long` | as the picture | – | `CobolValues.decodeBinary` (big-endian, two's complement if signed). |
| zoned `S9` with overpunch | `BigDecimal` | as the picture | – | `CobolValues.decodeZoned` / `encodeZoned`. |
| `FILLER` | – | dropped | – | Padding only; re-created by `padRight` when a fixed-width record is written. |
| KSDS primary key | `@Id` | `PRIMARY KEY` | path variable | |
| AIX (CARDAIX, CXACAIX, TRANSACT AIX) | repository finder | secondary index | lookup endpoint | §6. |
| `READ ... UPDATE` + `REWRITE` | `@Version` | `version BIGINT` | – | Conflict → `RecordChangedException` / HTTP 409 (§7). |
| `FILE STATUS` / CICS `RESP` | typed exception | – | `ApiError` | §8. |

## 2. Money: `PIC S9(n)V99` → `NUMERIC(n+2,2)` / `BigDecimal`, truncating

Fact: COBOL `MOVE`, `ADD`, `SUBTRACT` and `COMPUTE` without `ROUNDED` drop digits beyond the
receiving field's scale (truncation toward zero). CardDemo has no `ROUNDED` phrase on any money
computation (`grep -n ROUNDED app/cbl/*.cbl` returns nothing).

- `CBACT04C` line 464: `COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200` into
  `WS-MONTHLY-INT PIC S9(09)V99`. The branch used `RoundingMode.HALF_UP`;
  `InterestCalculationService` now uses `CobolValues.divide(..., 2)` (truncate).
  `1234.56 × 12.00 / 1200 = 12.3456` → **12.34** (HALF_UP gave 12.35); negative balances truncate
  toward zero (`-12.3456` → `-12.34`). Covered by
  `InterestCalculationServiceTest.truncatesTheMonthlyInterestLikeTheCobolComputeWithoutRounded`.
- Additions and subtractions of two 2-decimal values (`CBTRN02C` `2700-UPDATE-ACCOUNT-REC`,
  `2800-UPDATE-TCATBAL`) are exact in `BigDecimal`; no rounding mode is involved.
- Precision: `NUMERIC(n+2,2)` holds exactly what the picture holds. Overflow in COBOL silently drops
  high-order digits (no `ON SIZE ERROR` in CardDemo); in PostgreSQL it raises an error.
  **Hypothesis:** no CardDemo data path reaches the high-order limit, so failing loudly is
  preferred over reproducing silent high-order truncation.
- Inputs: the online programs accept amounts through edited screen fields. REST requests now reject
  values that do not fit the target picture (`@Digits`, HTTP 400) rather than truncating them
  silently, because a REST client never sees the screen echo that would reveal the truncation.
  This is a deliberate deviation. The limit is the picture of the field the value lands in: a
  posting to an account (`PostingRequest`) is `ACCT-CURR-BAL`-sized, `@Digits(integer = 10)`;
  a transaction amount is `TRAN-AMT`-sized, `@Digits(integer = 9)`.
- Fact: `COBIL00C` 224 moves the full `ACCT-CURR-BAL` (`S9(10)V99`) into `TRAN-AMT` (`S9(09)V99`),
  which silently drops the high-order digit for a balance of 1,000,000,000.00 or more. The port does
  not reproduce that: such a payment fails on the `NUMERIC(11,2)` transaction column instead.
  **Hypothesis:** no account reaches that balance; the gap is left to the bill-payment parity ticket.

Testing note: tests run against PostgreSQL 16 through Testcontainers with the real Flyway
migrations (§9), so `NUMERIC` scale, overflow and `CHECK` constraints behave as in production.

## 3. `PIC 9(n)` ids keep their leading zeros

Fact: `ACCT-ID PIC 9(11)` (CVACT01Y), `CUST-ID PIC 9(09)` (CVCUS01Y), `XREF-ACCT-ID`/`XREF-CUST-ID`
(CVACT03Y), `CARD-ACCT-ID` (CVACT02Y) and `TRAN-MERCHANT-ID PIC 9(09)` (CVTRA05Y) are unsigned
zoned decimals; the data files store them zero-filled (`00000000001` is the first account in
`app/data/ASCII/acctdata.txt`) and the screens display them that way.

The branch stored them as `BIGINT` / `Long` and serialized them as JSON numbers, so account
`00000000001` came back as `1`. Decision:

- Storage stays numeric (`BIGINT` with a `CHECK` on the picture range). Numeric ordering equals the
  KSDS key order for zero-filled fixed-width digits, the AIX lookups stay integer comparisons, and
  no migration of keys is needed.
- The width is restored at every boundary that shows the value: JSON responses use
  `@PicId(digits = n)` (account 11, customer 9, merchant 9), which writes
  `"accountId": "00000000001"`; fixed-width output uses `CobolValues.zeroPad(value, n)`.
- Every input path is range-checked to the picture (`@Min`/`@Max` on request DTOs, a `CHECK` on the
  column, e.g. `transaction` `V4__merchant_id_range.sql`), so `@PicId` never meets a value it cannot
  render.
- Request bodies and path variables accept either form (`"00000000001"` or `1`) because Jackson and
  Spring parse both into `Long`.
- `PIC 9(n)` fields that are codes rather than identifiers (`TRAN-CAT-CD PIC 9(04)`,
  `CUST-FICO-CREDIT-SCORE PIC 9(03)`) stay numbers without padding.
- 16-digit card numbers (`CARD-NUM PIC X(16)`) and `TRAN-ID PIC X(16)` are alphanumeric in the
  COBOL and stay `String` / `VARCHAR(16)`.

Responses changed by this: `AccountResponse.accountId`, `CustomerResponse.customerId`,
`CardResponse.accountId`, `CardXrefResponse.customerId`/`accountId`,
`TransactionResponse.merchantId`.

## 4. `PIC X(n)` strings and `PIC X(10)` dates

- Load: `CobolValues.trim` removes the blank padding; an all-blank field becomes `null`.
- Output to any fixed-width record (extracts, reports, statement files): `CobolValues.padRight(value, n)`
  pads with blanks and cuts to `n`, matching a COBOL `MOVE` into a shorter `PIC X` (left-justified,
  right-truncated).
- Column width is the picture width (`VARCHAR(n)`); request DTOs carry `@Size(max = n)`.

`PIC X(10)` dates become `DATE` only where the COBOL treats them as dates:

| Field | COBOL use (fact) | Type |
|---|---|---|
| `ACCT-OPEN-DATE`, `ACCT-EXPIRAION-DATE`, `ACCT-REISSUE-DATE` | `COACTUPC` 1479-1505 validate each with `EDIT-DATE-CCYYMMDD`; `CBTRN02C` 414 compares `ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)` | `DATE` |
| `CARD-EXPIRAION-DATE` | `COCRDUPC` splits it into year/month and validates the year (`VALID-YEAR VALUES 1950 THRU 2099`) | `DATE` |
| `CUST-DOB-YYYY-MM-DD` | `COACTUPC` splits and edits the date of birth (`ACUP-*-CUST-DOB-*`) | `DATE` |
| `ACCT-ADDR-ZIP`, `ACCT-GROUP-ID`, `TRAN-SOURCE`, `TRAN-MERCHANT-ZIP` | `PIC X(10)` text, no date use | `VARCHAR(10)` |

The COBOL comparisons are on `YYYY-MM-DD` text, which orders the same as the date, so comparing
`LocalDate` values is equivalent. Blank or invalid legacy dates load as `null`
(`CobolValues.date`). **Hypothesis:** no program relies on storing a non-date string in these
fields; the data files contain only valid dates for them.

Timestamps (`PIC X(26)`): `CBTRN02C` writes `TRAN-PROC-TS` from `Z-GET-DB2-FORMAT-TIMESTAMP`
(`YYYY-MM-DD-HH.MM.SS.mmmmmm`) and copies `TRAN-ORIG-TS` from the daily file, whose data uses
`YYYY-MM-DD HH:MM:SS.ffffff` (`app/data/ASCII/dailytran.txt`). `CobolValues.timestamp` reads both;
`CobolValues.toDb2Timestamp` writes the DB2 form. `CBTRN03C` 173 filters on `TRAN-PROC-TS (1:10)`,
i.e. the date part.

## 5. Binary encodings: COMP-3, COMP/BINARY, sign overpunch

The relational model never stores encoded bytes; these helpers are for reading and writing
mainframe extracts (`CBEXPORT`/`CBIMPORT` use `CVEXPORT.cpy`; the ASCII seed files use zoned decimals).

- **Zoned decimal with sign overpunch** (every `S9` display field in the ASCII files): the last
  character carries the sign — `{`/`A`–`I` = +0..+9, `}`/`J`–`R` = −0..−9. Example from
  `acctdata.txt`: `00000001940{` = `+194.00`; from `dailytran.txt`: `0000005047G` = `+504.77`.
  `CobolValues.decodeZoned(field, scale)` / `encodeZoned(value, integerDigits, scale)`.
- **COMP-3** (packed decimal, e.g. `EXP-ACCT-CURR-BAL PIC S9(10)V99 COMP-3`, `EXP-TRAN-AMT PIC S9(09)V99 COMP-3`,
  `EXP-CUST-FICO-CREDIT-SCORE PIC 9(03) COMP-3`): two digits per byte, sign in the last nibble
  (`C`/`F`/`A`/`E` positive, `D`/`B` negative); length is `ceil((digits+1)/2)` bytes.
  `CobolValues.decodePacked(bytes, scale)`.
- **COMP / BINARY** (e.g. `EXP-CUST-ID PIC 9(09) COMP`, `EXP-ACCT-CURR-CYC-DEBIT PIC S9(10)V99 COMP`,
  `EXP-XREF-ACCT-ID PIC 9(11) COMP`): big-endian; 1–4 digits = 2 bytes, 5–9 = 4 bytes,
  10–18 = 8 bytes; two's complement when signed. `CobolValues.decodeBinary(bytes, signed, scale)`.
  **Hypothesis:** the extracts are produced with the default `TRUNC(STD)` behavior, so binary
  values never exceed the picture's digit count.

EBCDIC-to-ASCII conversion of `PIC X` and zoned fields happens before these helpers (the files in
`app/data/ASCII` are already converted); packed and binary fields must be transferred as binary,
not code-page converted.

**Record layouts** (`com.carddemo.common.record`): one class per copybook under `record.layout`
(`AccountRecordLayout` = CVACT01Y, … `SecUserDataLayout` = CSUSR01Y, `ExportRecordLayout` = CVEXPORT
with its REDEFINES views) declares every field with its COBOL name and PICTURE. `RecordReader` reads
a file into `CobolRecord`s (line sequential for `app/data/ASCII`, fixed blocks for `app/data/EBCDIC`),
`CobolRecord` gets and sets typed values in `CodePage.ASCII` or `CodePage.EBCDIC` (CP037) and
`RecordWriter` writes them back. `SampleDataset` maps each dataset to its layout and files.
`SampleDataRoundTripTest` reads every file in `app/data`, decodes every field, re-encodes it and gets
the same bytes back; `AsciiEbcdicEquivalenceTest` compares the two copies field by field. Two source
differences are expected, both in the data files rather than the decoding: `acctdata.txt` record 49
has `ACCT-ADDR-ZIP` = `A000000000` where `ACCTDATA.PS` has `ZEROAPR`, and `discgrp.txt` record 34 has
`DIS-INT-RATE` 0.00 where `DISCGRP.PS` has 15.00. `ACCDATA.PS` is a byte-identical copy of
`ACCTDATA.PS`; `DALYTRAN.PS.INIT` is one all-zero 350-byte record, not data.

## 6. Alternate indexes → indexes or lookup endpoints

| VSAM AIX (fact: `app/jcl`) | Key | PostgreSQL | Endpoint |
|---|---|---|---|
| CARDDATA AIX, CICS file `CARDAIX` (`CARDFILE.jcl`, `KEYS(11 16)`: `CARD-ACCT-ID`, non-unique) | account id | `ix_cards_acct_id` on `card.cards(acct_id)` | `GET /api/v1/cards?accountId=` |
| CARDXREF AIX, CICS file `CXACAIX` (`XREFFILE.jcl`, `KEYS(11,25)`: `XREF-ACCT-ID`, non-unique) | account id | `ix_card_xref_acct_id` on `card.card_xref(acct_id)` | `GET /api/v1/card-xrefs/by-account/{accountId}` |
| TRANSACT AIX (`TRANIDX.jcl`, `KEYS(26 304)`: `TRAN-PROC-TS`, non-unique) | processed timestamp | `ix_transactions_proc_ts` | report date range (TRANREPT selects by `TRAN-PROC-TS` date) |

Rules: a non-unique AIX becomes a non-unique B-tree index plus a repository finder returning a
list; a browse (`STARTBR`/`READNEXT`) over an AIX becomes a paged, ordered query on the same index.
The transaction AIX index is added with the reports port (CBTRN03C / CORPT00C), where its only
readers are.

## 7. `READ ... UPDATE` → `@Version`

Fact: `COACTUPC` and `COCRDUPC` read with `READ ... UPDATE`, compare the record with the copy
taken when the screen was shown, and reject with `'Record changed by some one else. Please review'`
(`COACTUPC` 522, `COCRDUPC` 208). `COUSR02C` (328) and `COUSR03C` (275) `READ ... UPDATE` before
`REWRITE`/`DELETE`.

- Every entity rewritten by an online program has `@Version long version` mapped to a
  `version BIGINT NOT NULL DEFAULT 0` column: `Account`, `Customer`, `Card`, and now `User`
  (`auth` migration `V3__users_version.sql`).
- A version conflict (`OptimisticLockingFailureException`) or an explicit
  `RecordChangedException` is answered `409` with the COBOL message, by `RestExceptionHandler`.
- **Hypothesis:** to reproduce the "changed since you looked" check across two HTTP requests, the
  update DTOs must carry the version the client read; today they do not, so only concurrent
  updates inside one request window are caught. Adding the version to the update contract belongs
  to the online-program parity tickets.

## 8. `FILE STATUS` / CICS `RESP` → typed exceptions

| COBOL outcome (fact) | Java | HTTP |
|---|---|---|
| status `00` / `DFHRESP(NORMAL)` | normal return | 200/201 |
| status `10` (end of file) in batch | end of the reader / stream | – |
| status `23` / `INVALID KEY` / `DFHRESP(NOTFND)` (e.g. `COACTVWC`, `COBIL00C`, `COTRN01C`) | `NotFoundException` | 404 |
| status `22` / `DFHRESP(DUPREC)` / `DFHRESP(DUPKEY)` (e.g. `COBIL00C`, `COTRN02C`, `COUSR01C`) | `DuplicateKeyException` | 409 |
| record changed after `READ ... UPDATE` | `RecordChangedException` / `OptimisticLockingFailureException` | 409 |
| business validation (reason codes 100–103 in `CBTRN02C`, screen edits) | `BusinessRuleException` / bean validation | 422 / 400 |
| any other status in batch → `9910-DISPLAY-IO-STATUS` + `9999-ABEND-PROGRAM` (`CEE3ABD`), e.g. `CBTRN02C`, `CBACT04C` | unchecked exception that fails the job step (no catch) | 500 |
| `DFHRESP(OTHER)` online | unchecked exception | 500 |

Batch jobs do not swallow unexpected I/O errors: an abend in COBOL is a failed Spring Batch step,
not a skipped record. Missing-record cases that the COBOL abends on (e.g. `CBACT04C`
`1100-GET-ACCT-DATA`) are open parity gaps listed in architecture.md §4.2.

## 9. Tests use PostgreSQL, not H2

All five services' test profiles (`src/test/resources/application.yml`) use the Testcontainers
JDBC URL `jdbc:tc:postgresql:16:///carddemo` (the image docker-compose runs), Flyway enabled on
the service schema, and `ddl-auto: validate`. Consequences:

- The Flyway SQL, `CHECK` constraints and `NUMERIC` precision are exercised by `mvn verify`, not
  only by `docker compose up`. The migrations create empty tables (no seed rows); tests insert what
  they need, and the `data-loader` module's `SampleDataReconciliationTest` loads all of `app/data`
  into the five schemas and checks the result (`docs/migration/validation/`).
- Hibernate mappings are validated against the migrated schema.
- Docker is required to run the tests (GitHub-hosted runners have it). `testcontainers.version`
  is pinned to 1.21.4 in the root POM because the 1.19.8 managed by Spring Boot 3.3.4 uses a Docker
  API version that Docker Engine 29 rejects.

## 10. Worked example: ACCOUNT-RECORD (`CVACT01Y`, ACCTDATA, RECLN 300)

First record of `app/data/ASCII/acctdata.txt` (asserted in `CobolValuesTest.workedExampleAccountRecord`):

| Field | Picture | Offset/len | Raw | Java (`Account`) | Column (`account.accounts`) | JSON |
|---|---|---|---|---|---|---|
| `ACCT-ID` | `9(11)` | 0/11 | `00000000001` | `Long 1` | `acct_id BIGINT` PK, `CHECK 0..99999999999` | `"accountId": "00000000001"` |
| `ACCT-ACTIVE-STATUS` | `X(01)` | 11/1 | `Y` | `String "Y"` | `active_status VARCHAR(1)` | `"Y"` |
| `ACCT-CURR-BAL` | `S9(10)V99` | 12/12 | `00000001940{` | `BigDecimal 194.00` | `curr_bal NUMERIC(12,2)` | `194.00` |
| `ACCT-CREDIT-LIMIT` | `S9(10)V99` | 24/12 | `00000020200{` | `2020.00` | `credit_limit NUMERIC(12,2)` | `2020.00` |
| `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | 36/12 | `00000010200{` | `1020.00` | `cash_credit_limit NUMERIC(12,2)` | `1020.00` |
| `ACCT-OPEN-DATE` | `X(10)` | 48/10 | `2014-11-20` | `LocalDate` | `open_date DATE` | `"2014-11-20"` |
| `ACCT-EXPIRAION-DATE` | `X(10)` | 58/10 | `2025-05-20` | `LocalDate` | `expiration_date DATE` | |
| `ACCT-REISSUE-DATE` | `X(10)` | 68/10 | `2025-05-20` | `LocalDate` | `reissue_date DATE` | |
| `ACCT-CURR-CYC-CREDIT` | `S9(10)V99` | 78/12 | `00000000000{` | `0.00` | `curr_cyc_credit NUMERIC(12,2)` | |
| `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | 90/12 | `00000000000{` | `0.00` | `curr_cyc_debit NUMERIC(12,2)` | |
| `ACCT-ADDR-ZIP` | `X(10)` | 102/10 | `A000000000` | `String` (trimmed) | `addr_zip VARCHAR(10)` | |
| `ACCT-GROUP-ID` | `X(10)` | 112/10 | blanks | `String` (trimmed) | `group_id VARCHAR(10)`, `ix_accounts_group_id` | |
| `FILLER` | `X(178)` | 122/178 | blanks | – | – | – |
| *(new)* | | | | `@Version long version` | `version BIGINT` | – |

Posting (`CBTRN02C` 2700): `curr_bal += amount`, then `curr_cyc_credit += amount` if
`amount >= 0` else `curr_cyc_debit += amount` — exact `BigDecimal` addition at scale 2. Updates via
`PUT /accounts/{id}` rely on `@Version` (§7).

## 11. Worked example: TRAN-RECORD (`CVTRA05Y`, TRANSACT, RECLN 350)

Built and re-parsed in `CobolValuesTest.workedExampleTranRecordRoundTrip` from the first record of
`app/data/ASCII/dailytran.txt` (same layout, CVTRA06Y):

| Field | Picture | Offset/len | Raw | Java (`Transaction`) | Column (`transaction.transactions`) |
|---|---|---|---|---|---|
| `TRAN-ID` | `X(16)` | 0/16 | `0000000000683580` | `String` | `tran_id VARCHAR(16)` PK |
| `TRAN-TYPE-CD` | `X(02)` | 16/2 | `01` | `String` | `type_cd VARCHAR(2)` |
| `TRAN-CAT-CD` | `9(04)` | 18/4 | `0001` | `Integer 1` (code, not padded) | `cat_cd INTEGER` |
| `TRAN-SOURCE` | `X(10)` | 22/10 | `POS TERM  ` | `"POS TERM"` | `source VARCHAR(10)` |
| `TRAN-DESC` | `X(100)` | 32/100 | `Purchase at Abshire-Lowe…` | trimmed | `description VARCHAR(100)` |
| `TRAN-AMT` | `S9(09)V99` | 132/11 | `0000005047G` | `BigDecimal 504.77` | `amount NUMERIC(11,2)` |
| `TRAN-MERCHANT-ID` | `9(09)` | 143/9 | `800000000` | `Long` → JSON `"800000000"` | `merchant_id BIGINT` |
| `TRAN-MERCHANT-NAME` | `X(50)` | 152/50 | `Abshire-Lowe…` | trimmed | `merchant_name VARCHAR(50)` |
| `TRAN-MERCHANT-CITY` | `X(50)` | 202/50 | `North Enoshaven…` | trimmed | `merchant_city VARCHAR(50)` |
| `TRAN-MERCHANT-ZIP` | `X(10)` | 252/10 | `72112     ` | `"72112"` | `merchant_zip VARCHAR(10)` |
| `TRAN-CARD-NUM` | `X(16)` | 262/16 | `4859452612877065` | `String` | `card_num VARCHAR(16)`, `ix_transactions_card_num` |
| `TRAN-ORIG-TS` | `X(26)` | 278/26 | `2022-06-10 19:27:53.000000` | `LocalDateTime` | `orig_ts TIMESTAMP` |
| `TRAN-PROC-TS` | `X(26)` | 304/26 | blanks in the daily file; DB2 format once posted | `LocalDateTime` / `null` | `proc_ts TIMESTAMP` (AIX key, §6) |
| `FILLER` | `X(20)` | 330/20 | blanks | – | – |

Writing the record back (fixed-width extract or report): `padRight` each `X(n)`,
`encodeZoned(amount, 9, 2)`, `zeroPad(merchantId, 9)`, `toDb2Timestamp` for timestamps, then
`padRight("", 20)` for the FILLER. Interest transactions created by `CBACT04C` carry the truncated
amount from §2.

## 12. Decisions that changed from the branch

- Interest truncates instead of rounding `HALF_UP` (architecture.md §4.2 finding, now fixed in
  code; the other two CBACT04C differences remain open).
- `PIC 9(n)` ids are emitted as zero-padded strings instead of JSON numbers. This is a breaking
  change for API clients that compared ids numerically.
- Tests run on PostgreSQL with Flyway instead of H2 with Hibernate DDL.
- `COPYBOOK-TO-SCHEMA-MAPPING.md` listed `PIC 9(n)` as `NUMERIC(n,0)` and dates as `DATE`
  unconditionally and `X(16)` keys as `CHAR(16)`; the migrations actually use `BIGINT`/`INTEGER` and `VARCHAR(16)`. The table there is corrected and this document is now the rule.
