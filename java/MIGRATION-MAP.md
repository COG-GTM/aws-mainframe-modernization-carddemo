# CardDemo COBOL → Java 21 migration map

Phase 0 delivers the build, the data model and the parity harness only. Every row below names the target of a
later workstream; **no business logic has been migrated yet**. "Status: P0" means the artefact is already covered
by Phase 0 code, "todo" means it belongs to a later workstream.

Target modules (Maven, `java/`):

| Module | Role |
|:--|:--|
| `carddemo-common` | COBOL data types (`cobol`), copybook parser/layouts (`copybook`), dataset I/O (`io`), record codec (`codec`); golden-master harness in the test-jar (`testing`) |
| `carddemo-data` | JPA entities, Spring Data repositories, Flyway schema, copybook records + MapStruct mappers, dataset loader |
| `carddemo-online` | CICS/BMS replacement (Spring Boot web) |
| `carddemo-batch` | JCL/batch replacement (Spring Batch) |

Package abbreviations: `online` = `com.carddemo.online`, `batch` = `com.carddemo.batch`,
`data` = `com.carddemo.data`, `common` = `com.carddemo.common`.

## 1. Online programs (CICS) — `app/cbl`, `app/bms`, `app/cpy-bms`

| Tran | Program | BMS map (symbolic copybook) | Function | Data used (CICS files) | Target class (module) | Status |
|:--|:--|:--|:--|:--|:--|:--|
| CC00 | COSGN00C | COSGN00 (COSGN00.CPY) | Sign-on | USRSEC | `online.signon.SignonController` + `SignonService` | todo |
| CM00 | COMEN01C | COMEN01 (COMEN01.CPY) | Main menu (COMEN02Y option table) | – | `online.menu.MainMenuController` | todo |
| CA00 | COADM01C | COADM01 (COADM01.CPY) | Admin menu (COADM02Y option table) | – | `online.menu.AdminMenuController` | todo |
| CAVW | COACTVWC | COACTVW (COACTVW.CPY) | Account view | ACCTDAT, CUSTDAT, CXACAIX | `online.account.AccountViewController` + `AccountQueryService` | todo |
| CAUP | COACTUPC | COACTUP (COACTUP.CPY) | Account update (CSUTLDPY/CSLKPCDY validations) | ACCTDAT, CUSTDAT, CXACAIX | `online.account.AccountUpdateController` + `AccountUpdateService` | todo |
| CCLI | COCRDLIC | COCRDLI (COCRDLI.CPY) | Card list (browse) | CARDDAT | `online.card.CardListController` | todo |
| CCDL | COCRDSLC | COCRDSL (COCRDSL.CPY) | Card view | CARDDAT, CARDAIX | `online.card.CardViewController` | todo |
| CCUP | COCRDUPC | COCRDUP (COCRDUP.CPY) | Card update | CARDDAT | `online.card.CardUpdateController` + `CardUpdateService` | todo |
| CT00 | COTRN00C | COTRN00 (COTRN00.CPY) | Transaction list (browse) | TRANSACT | `online.transaction.TransactionListController` | todo |
| CT01 | COTRN01C | COTRN01 (COTRN01.CPY) | Transaction view | TRANSACT | `online.transaction.TransactionViewController` | todo |
| CT02 | COTRN02C | COTRN02 (COTRN02.CPY) | Transaction add (calls CSUTLDTC) | TRANSACT, CCXREF, CXACAIX | `online.transaction.TransactionAddController` + `TransactionAddService` | todo |
| CR00 | CORPT00C | CORPT00 (CORPT00.CPY) | Report request (submits TRANREPT via TDQ/internal reader) | – | `online.report.ReportRequestController` → launches `batch` job `transactionReportJob` | todo |
| CB00 | COBIL00C | COBIL00 (COBIL00.CPY) | Bill payment | ACCTDAT, CXACAIX, TRANSACT | `online.billing.BillPaymentController` + `BillPaymentService` | todo |
| CU00 | COUSR00C | COUSR00 (COUSR00.CPY) | User list (browse) | USRSEC | `online.user.UserListController` | todo |
| CU01 | COUSR01C | COUSR01 (COUSR01.CPY) | User add | USRSEC | `online.user.UserAddController` | todo |
| CU02 | COUSR02C | COUSR02 (COUSR02.CPY) | User update | USRSEC | `online.user.UserUpdateController` | todo |
| CU03 | COUSR03C | COUSR03 (COUSR03.CPY) | User delete | USRSEC | `online.user.UserDeleteController` | todo |
| – | CSUTLDTC | – | Date validation (wraps CEEDAYS), CALLed by CORPT00C/COTRN02C | – | `common.cobol` date helper `CobolDateValidator` (common) | todo |

Cross-cutting online artefacts: COCOM01Y (COMMAREA) → `online.session.CardDemoContext` (server-side session /
request context); COTTL01Y, CSDAT01Y, CSMSG01Y, CSMSG02Y (titles, date/time, messages, abend data) → `online.web`
view helpers; CSSTRPFY (PF-key mapping) → UI navigation actions; CSSETATY (field attribute macro) → form error
highlighting; CVCRD01Y (card work area) → controller form objects; DFHAID/DFHBMSCA → not needed.

## 2. Batch programs — `app/cbl`, `app/jcl`, `app/proc`

| JCL job | Program | Function | Input → output | Target (module) | Status |
|:--|:--|:--|:--|:--|:--|
| POSTTRAN | CBTRN02C | Post daily transactions, reject invalid ones | DALYTRAN, CCXREF, ACCTDAT, TCATBALF → TRANSACT, DALYREJS | `batch.posting.PostTransactionsJobConfig` | todo |
| INTCALC | CBACT04C | Interest & fee calculation | TCATBALF, CCXREF, DISCGRP, ACCTDAT → TRANSACT (SYSTRAN) | `batch.interest.InterestCalculationJobConfig` | todo |
| TRANREPT (+ PROC TRANREPT) | CBTRN03C | Transaction detail report (CVTRA07Y layout) | TRANSACT (sorted), CCXREF, TRANTYPE, TRANCATG, DATEPARM → TRANREPT | `batch.report.TransactionReportJobConfig` (`NumericEdited` for report pictures) | todo |
| CREASTMT | CBSTM03A + CBSTM03B | Customer statements (text + HTML); CBSTM03B is the file-access subroutine | TRNXFILE (COSTM01), XREF, CUST, ACCT → STMTFILE, HTMLFILE | `batch.statement.StatementJobConfig` (+ `StatementFileAccess`) | todo |
| – (CBTRN01C) | CBTRN01C | Daily transaction validation driver (reads DALYTRAN, looks up XREF/ACCT/CARD/CUST) | DALYTRAN → display | `batch.posting.DailyTransactionValidationStep` | todo |
| READACCT | CBACT01C | Account file reader/extract (calls COBDATFT assembler for dates; writes OUTFILE, ARRYFILE, VBRCFILE incl. VB records) | ACCTDAT → 3 extracts | `batch.extract.AccountExtractJobConfig` (`RecordFormat.VARIABLE`) | todo |
| READCARD | CBACT02C | Card file print | CARDDAT → SYSOUT | `batch.extract.CardListingJobConfig` | todo |
| READXREF | CBACT03C | Xref file print | CCXREF → SYSOUT | `batch.extract.XrefListingJobConfig` | todo |
| READCUST | CBCUS01C | Customer file print | CUSTDAT → SYSOUT | `batch.extract.CustomerListingJobConfig` | todo |
| WAITSTEP | COBSWAIT (+ MVSWAIT asm) | Sleep for n centiseconds | PARM | `batch.util.WaitTasklet` | todo |
| COMBTRAN | SORT + IDCAMS | Merge TRANSACT backup + SYSTRAN, reload KSDS | – | `batch.posting.CombineTransactionsJobConfig` (SQL, no program) | todo |
| ACCTFILE, CARDFILE, CUSTFILE, XREFFILE, TRANFILE, DISCGRP, TCATBALF, TRANCATG, TRANTYPE, DUSRSECJ | IDCAMS REPRO / IEBGENER | Initial load of VSAM files from PS datasets | PS → KSDS | `data.load.CardDemoDataImporter` (+ Flyway schema) | **P0** |
| TRANBKP, TRANIDX, DALYREJS, REPTFILE, DEFGDGB, DEFGDGD, DEFCUST, ESDSRRDS, PRTCATBL (+ PROC REPROC) | IDCAMS / SORT | Dataset definitions, AIX, GDG bases, backups | – | Flyway DDL / indexes; GDG generations → timestamped output files or tables | P0 (DDL) / todo |
| CLOSEFIL, OPENFIL | IEFBR14 / SDSF CEMT | Close/open files in CICS around batch | – | Not needed (shared RDBMS); replaced by job-level locking if required | n/a |
| CBADMCDJ | DFHCSDUP | CICS resource definitions (app/csd) | – | Not needed (Spring config) | n/a |
| INTRDRJ1, INTRDRJ2 | IEBGENER / IDCAMS | Internal reader demo (job submits job) | – | Scheduler / `JobOperator` launch | todo |
| FTPJCL, TXT2PDF1 | FTP / IKJEFT1B | File transfer, text → PDF utilities | – | Infrastructure (SFTP, PDF library) | todo |

Scheduler definitions `app/scheduler/CardDemo.ca7` (and Control-M where present) → job ordering for the batch
workstream (Spring Batch jobs launched by the enterprise scheduler or `CardDemoBatchApplication` CLI).
Assembler `app/asm/COBDATFT.asm`, `MVSWAIT.asm` and macros in `app/maclib` → `java.time` formatting and
`Thread.sleep`; no Java port of assembler. `app/ctl/REPROCT.ctl` → `CardDemoDataImporter`.

## 3. Copybooks — `app/cpy`

| Copybook | Content | Target | Status |
|:--|:--|:--|:--|
| CVACT01Y | ACCOUNT-RECORD (300) | `data.entity.Account`, `data.record.AccountRecord`, table `account` | **P0** |
| CVACT02Y | CARD-RECORD (150) | `data.entity.Card`, `data.record.CardRecord`, table `card` (+ CARDAIX index) | **P0** |
| CVACT03Y | CARD-XREF-RECORD (50) | `data.entity.CardCrossReference`, `data.record.CardXrefRecord`, table `card_xref` (+ CXACAIX index) | **P0** |
| CVCUS01Y | CUSTOMER-RECORD (500) | `data.entity.Customer` (+ embedded `Address`), `data.record.CustomerRecord`, table `customer` | **P0** |
| CUSTREC | Alternate CUSTOMER-RECORD used by CBSTM03A (DOB named CUST-DOB-YYYYMMDD) | Same entity `Customer`; layout identical byte-for-byte | P0 (via CVCUS01Y) |
| CVTRA01Y | TRAN-CAT-BAL-RECORD (50) | `data.entity.TransactionCategoryBalance` (+ `@EmbeddedId`), table `tran_cat_balance` | **P0** |
| CVTRA02Y | DIS-GROUP-RECORD (50) | `data.entity.DisclosureGroup` (+ `@EmbeddedId`), table `disclosure_group` | **P0** |
| CVTRA03Y | TRAN-TYPE-RECORD (60) | `data.entity.TransactionType`, table `transaction_type` | **P0** |
| CVTRA04Y | TRAN-CAT-RECORD (60) | `data.entity.TransactionCategory` (+ `@EmbeddedId`), table `transaction_category` | **P0** |
| CVTRA05Y | TRAN-RECORD (350) | `data.entity.Transaction` (`AbstractTransaction`), table `transactions` | **P0** |
| CVTRA06Y | DALYTRAN-RECORD (350) | `data.entity.DailyTransaction` (`AbstractTransaction`), table `daily_transactions` | **P0** |
| COSTM01 | TRNX-RECORD: TRAN-RECORD re-keyed by card + id (statement input) | `batch.statement` read model over `transactions` | todo |
| CVTRA07Y | Transaction report lines (numeric-edited pictures) | `batch.report` line formatter (`common.cobol.NumericEdited`) | todo |
| CSUSR01Y | SEC-USER-DATA (80) | `data.entity.UserSecurity` (+ `UserType` enum), table `user_security` | **P0** |
| UNUSED1Y | Unused copy of the user layout | None (dead code) | n/a |
| COCOM01Y | CARDDEMO-COMMAREA | `online.session.CardDemoContext` | todo |
| COMEN02Y, COADM02Y | Menu option tables (REDEFINES + OCCURS 12 / 9) | `online.menu` option enums | todo |
| COTTL01Y, CSDAT01Y, CSMSG01Y, CSMSG02Y | Screen titles, current date/time, messages, abend data | `online.web` helpers | todo |
| CSUTLDWY, CSUTLDPY | Date edit working storage + procedure (REDEFINES numeric views) | `common.cobol.CobolDateValidator` | todo |
| CSLKPCDY | Lookup tables: phone area codes, US states, state/zip prefixes (88-levels) | `online.account.validation` reference data | todo |
| CSSTRPFY, CSSETATY | PF-key store / attribute-setting procedure snippets | UI navigation / error highlighting | todo |
| CVCRD01Y | Card screens work area (REDEFINES text/numeric ids) | Controller form objects | todo |
| CODATECN | COBDATFT parameter block (REDEFINES date formats) | `java.time` | todo |
| CVEXPORT (only on newer upstream) | Export record | – | not in this repo |

BMS symbolic maps `app/cpy-bms/*.CPY` and maps `app/bms/*.bms` → one view model + template/API resource per map
(see section 1). Field inventory per map is parsed by `CopybookParser` (all `cpy-bms` members parse).

## 4. Optional modules

### 4.1 `app/app-authorization-ims-db2-mq` (IMS DB + DB2 + MQ)

| Artefact | Function | Target | Status |
|:--|:--|:--|:--|
| CIPAUSMY | IMS root segment PAUTSUM0 (COMP-3, COMP, OCCURS 5) | `data.entity.PendingAuthorizationSummary` (+ `@ElementCollection` statuses), table `pending_auth_summary` | **P0** |
| CIPAUDTY | IMS child segment PAUTDTL1 (COMP-3 key/amounts) | `data.entity.PendingAuthorizationDetail` (+ `@EmbeddedId`), table `pending_auth_detail` | **P0** |
| CCPAURQY, CCPAURLY, CCPAUERY | MQ authorization request / reply / error messages | `online.authorization.messaging` DTOs (JMS/Kafka) | todo |
| COPAUA0C (CP00) | MQ-triggered authorization decision; updates IMS | `online.authorization.AuthorizationListener` + `AuthorizationService` | todo |
| COPAUS0C (CPVS) / COPAU00 | Pending authorization summary screen | `online.authorization.PendingAuthSummaryController` | todo |
| COPAUS1C (CPVD) / COPAU01 | Pending authorization detail screen, mark fraud | `online.authorization.PendingAuthDetailController` | todo |
| COPAUS2C | Insert fraud record into DB2 AUTHFRDS | `online.authorization.FraudReportService` + entity `AuthorizationFraud` (DDL `AUTHFRDS.ddl`) | todo |
| CBPAUP0C / CBPAUP0J | Purge expired authorizations (BMP) | `batch.authorization.PurgeExpiredAuthorizationsJobConfig` | todo |
| PAUDBUNL, PAUDBLOD, DBUNLDGS / UNLDPADB, LOADPADB, UNLDGSAM | IMS unload/load utilities | `data.load` loader for IMS unload files (`RecordFormat.VARIABLE`) | todo |
| DBPAUTP0.dbd, DBPAUTX0.dbd, PADFLDBD/PASFLDBD, *.psb, PAUTBPCB/PADFLPCB/PASFLPCB, IMSFUNCS | IMS DBD/PSB/PCB definitions | Flyway `V2__pending_authorizations.sql` (hierarchy → FK) | **P0** (schema) |
| AUTHFRDS.ddl/.dcl, XAUTHFRD.ddl | DB2 fraud table | Flyway migration in the authorization workstream | todo |
| COPAU00.cpy, COPAU01.cpy | Symbolic maps | view models | todo |

### 4.2 `app/app-transaction-type-db2` (DB2)

| Artefact | Function | Target | Status |
|:--|:--|:--|:--|
| TRNTYPE.ddl / DCLTRTYP | DB2 TRANSACTION_TYPE | `data.entity.TransactionType` (same table as the VSAM TRANTYPE file) | **P0** |
| TRNTYCAT.ddl / DCLTRCAT | DB2 TRANSACTION_TYPE_CATEGORY (FK to type) | `data.entity.TransactionCategory`; FK added by this workstream | P0 / todo (FK) |
| COTRTLIC (CTLI) / COTRTLI | List/update/delete types (cursor, paging) | `online.admin.TransactionTypeListController` | todo |
| COTRTUPC (CTTU) / COTRTUP | Add/edit type | `online.admin.TransactionTypeEditController` | todo |
| COBTUPDT / MNTTRDB2 | Batch maintenance of types from an input file | `batch.admin.TransactionTypeMaintenanceJobConfig` | todo |
| CREADB21, TRANEXTR, ctl/*.ctl | Create/load DB2 tables, extract to VSAM | Flyway + `CardDemoDataImporter` | P0 (schema) / todo |
| CSDB2RWY, CSDB2RPY | DB2 error-handling working storage/procedure | Spring `DataAccessException` handling | todo |

### 4.3 `app/app-vsam-mq` (MQ)

| Artefact | Function | Target | Status |
|:--|:--|:--|:--|
| CODATE01 (CDRD) | System date inquiry via MQ request/reply | `online.messaging.SystemDateResponder` | todo |
| COACCT01 (CDRA) | Account inquiry via MQ request/reply (ACCTDAT) | `online.messaging.AccountInquiryResponder` (uses `AccountRepository`) | todo |

## 5. Data-type conventions (Phase 0)

| COBOL | Java / SQL |
|:--|:--|
| `PIC S9(n)V9(m)` DISPLAY (zoned, trailing overpunch) | `BigDecimal` scale m / `NUMERIC(n+m, m)`; codec `ZonedDecimal` |
| `PIC S9(n)V9(m) COMP-3` | `BigDecimal` / `NUMERIC`; codec `PackedDecimal` |
| `PIC S9(n) COMP` / `COMP-5` | `Integer`/`Long`; codec `CobolBinary` |
| `PIC 9(n)` key or id | `Long`/`Integer` (`BIGINT`/`INTEGER`), zero-padded on encode; SSN kept as text |
| `PIC X(n)` | `String` (trailing spaces trimmed, re-padded on encode) / `VARCHAR(n)` |
| `PIC X(10)` dates `YYYY-MM-DD`, `PIC X(26)` timestamps | `LocalDate` / `LocalDateTime` (`CobolFormats`), blank ↔ `null` |
| Group keys (TRAN-CAT-KEY, DIS-GROUP-KEY, PA-AUTHORIZATION-KEY) | `@EmbeddedId` classes; nested Java records in the codec |
| Adjacent related items (CUST-ADDR-*) | `@Embeddable Address` |
| `OCCURS n` | `List<...>` in records; `@ElementCollection` + `@OrderColumn` in entities |
| `REDEFINES` | Alternate views over the same bytes (`RecordView`, `CopybookField.isAlternateView()`); persisted via the primary view, alternate layouts (CUSTREC, COSTM01) map to the same entity/read model |
| Level-88 | Enums (`UserType`) or documented codes |
| FILLER | Not persisted |

## 6. Data findings surfaced by the golden master

* `ACCTDATA`: the account group id (`A000000000`, `ZEROAPR`) sits in **ACCT-ADDR-ZIP**, ACCT-GROUP-ID is blank in
  every record. Kept as-is; the interest workstream must decide how CBACT04C's DISCGRP lookup behaves.
* The ASCII copies in `app/data/ASCII` lag the EBCDIC datasets in two places (ACCTDATA record 49 ACCT-ADDR-ZIP,
  DISCGRP record 34 DIS-INT-RATE). The EBCDIC files are treated as the source of truth; both differences are
  declared `KnownDifference`s.
* `DISCGRP` and `TCATBALF` FILLER areas contain zeros, not spaces; FILLER is not persisted.
* `DALYTRAN`: TRAN-PROC-TS is blank in all 300 records (→ `null`).
* `CardDemoDataset` covers the documented PS datasets; `ACCDATA.PS` is a byte-identical duplicate of `ACCTDATA.PS`.
