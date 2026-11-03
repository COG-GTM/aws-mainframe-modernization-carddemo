# Coverage matrix: COBOL programs and JCL jobs → Java

Ticket UNT149-23 (phase: verification and handover). Every program in `app/cbl` (31, [inventory.md](inventory.md) §3)
and every member of `app/jcl` (38) and `app/proc` (2) ([jcl-mapping.md](jcl-mapping.md)), with its Java counterpart,
how it was verified and the status. The online flow-test table (§3) is generated from the test sources by
`scripts/migration/coverage-matrix.py`; CI fails when it is stale. Accepted deviations and open risks are in
[residual-risks.md](residual-risks.md).

## 1. Verification methods

| Method | What it proves | Where | Runs in CI |
|---|---|---|---|
| **Golden-set chain** | The unmodified COBOL under GnuCOBOL and the Java jobs on a freshly loaded database, Control-M order POSTTRAN → INTCALC → COMBTRAN → TRANREPT → CREASTMT, every output file and the account/category-balance after-images compared field by field; any unexplained difference fails | `modernization/verification/` (`run.sh`, per-job notes) | `golden-set` job |
| **Module golden set** | One program under GnuCOBOL vs its Java job, byte for byte, on the sample data plus edge sets | `batch-service` `GoldenSetTest`, `BranchRoundTripTest` ([validation/batch-service-golden-set.md](validation/batch-service-golden-set.md)); `IntcalcCobolParityTest`; TRANREPT 11 cases (`TranreptCases`, references from `scripts/migration/tranrept/`) | `verify` job |
| **Online flow tests** | Each CICS screen flow walked through the REST API on Testcontainers PostgreSQL with the sample data loaded; expected messages and behavior **read from the COBOL source** (no CICS here) | `@CobolFlow` tests, [validation/online-flow-tests.md](validation/online-flow-tests.md) | `verify` job |
| **Unit / job tests** | Java behavior checked against expectations written from the COBOL, without a COBOL run | named per row | `verify` job |
| **Data-definition** | The JCL only defines/loads data sets: Flyway schema + `data-loader`, reconciled field by field against `app/data` | `SampleDataReconciliationTest`, [validation/sample-data-reconciliation.md](validation/sample-data-reconciliation.md) | `verify` + `compose-smoke` |
| **Replaced / not migrated** | No Java port by decision; reason given | [scope.md](scope.md) §3 | — |

Status: **verified** = method ran and passes; **verified (source-derived)** = online flow tests pass, but the
expectations come from reading COBOL, not from running it; **partial** = runs, with a named gap; **planned** =
no Java job yet; **replaced** / **not migrated** = intentionally no port.

## 2. Programs (31)

### 2.1 CICS online (17)

| Program | Tran | Java counterpart | Verification | Status |
|---|---|---|---|---|
| COSGN00C | CC00 | auth-service `POST /api/v1/auth/signon` (bearer token replaces COMMAREA user fields) | online flow tests | verified (source-derived) |
| COMEN01C | CM00 | auth-service `GET /api/v1/menu`, `/menu/options/{n}` (U token) | online flow tests | verified (source-derived) |
| COADM01C | CA00 | auth-service `GET /api/v1/menu` (A token) | online flow tests | verified (source-derived) |
| COACTVWC | CAVW | account-service `GET /api/v1/accounts/{id}/view` | online flow tests; [account-view-update.md](account-view-update.md) | verified (source-derived) |
| COACTUPC | CAUP | account-service `PUT /api/v1/accounts/{id}` (+ customer-service) | online flow tests (all 25 `AccountUpdateValidator.RULES` messages) | verified (source-derived) |
| COCRDLIC | CCLI | card-service `GET /api/v1/cards` | online flow tests; [card-service-review.md](card-service-review.md) | verified (source-derived) |
| COCRDSLC | CCDL | card-service `GET /api/v1/cards/{n}` | online flow tests | verified (source-derived) |
| COCRDUPC | CCUP | card-service `PUT /api/v1/cards/{n}` | online flow tests | verified (source-derived) |
| COTRN00C | CT00 | transaction-service `GET /api/v1/transactions` | online flow tests | verified (source-derived) |
| COTRN01C | CT01 | transaction-service `GET /api/v1/transactions/{id}` | online flow tests | verified (source-derived) |
| COTRN02C | CT02 | transaction-service `POST /api/v1/transactions` | online flow tests | verified (source-derived) |
| CORPT00C | CR00 | batch-service `POST /api/v1/reports/transactions` → TRANREPT (async, polled) | online flow tests | verified (source-derived) |
| COBIL00C | CB00 | transaction-service bill payment + account-service `BILL_PAYMENT` posting | online flow tests | verified (source-derived) |
| COUSR00C | CU00 | auth-service `GET /api/v1/users` (admin) | online flow tests | verified (source-derived) |
| COUSR01C | CU01 | auth-service `POST /api/v1/users` | online flow tests | verified (source-derived) |
| COUSR02C | CU02 | auth-service `PUT /api/v1/users/{id}` | online flow tests | verified (source-derived) |
| COUSR03C | CU03 | auth-service `DELETE /api/v1/users/{id}` | online flow tests | verified (source-derived) |

Not a row of the 31: CSD transaction **CDV1 → COCRDSEC** has a `DEFINE PROGRAM` in `CARDDEMO.CSD` but no
source anywhere in the repository, so there is nothing to migrate or verify (not migrated). Menu option 11
(`COPAUS0C`) and admin options 5–6 (`COTRTLIC`, `COTRTUPC`) belong to the extensions and answer "not installed".

### 2.2 Batch programs and utilities (14)

| Program | Run by | Java counterpart | Verification | Status |
|---|---|---|---|---|
| CBTRN02C | POSTTRAN | transaction-service `POSTTRAN` | golden-set chain (TRANSACT, DALYREJS, TCATBALF, ACCTFILE vs `account.accounts`) | verified |
| CBACT04C | INTCALC | transaction-service `INTCALC` | golden-set chain (SYSTRAN, ACCTFILE, TCATBALF); `IntcalcCobolParityTest` | verified (keeps the last-account defect, see risks) |
| CBTRN03C | TRANREPT | batch-service `TRANREPT` | golden-set chain (TRANSACT.BKUP/DALY, TRANREPT); module golden set, 11 cases | verified |
| CBSTM03A | CREASTMT | batch-service `CREASTMT` (`StatementWriter`) | golden-set chain (TRXFL.SEQ, STMTFILE, HTMLFILE, compile shims checked); module golden set | verified |
| CBSTM03B | called by CBSTM03A | batch-service `StatementFileService` | as CBSTM03A | verified |
| CBEXPORT | CBEXPORT | batch-service `CBEXPORT` | module golden set (`GoldenSetTest`, `BranchRoundTripTest`) | verified |
| CBIMPORT | CBIMPORT | batch-service `CBIMPORT` | module golden set incl. unknown-type edge set | verified |
| CBTRN01C | no JCL member | transaction-service `CBTRN01C` (job keyed by program name) | `BatchJobsTest` (SYSOUT lines as the COBOL DISPLAYs, no updates); not in the golden-set chain | verified (job test) |
| CBACT01C | READACCT | batch-service `READACCT` (`COBDATFT` → `java.time`) | module golden set + edge set (OUTFILE, ARRYFILE, VBRCFILE, SYSOUT) | verified |
| CBACT02C | READCARD | batch-service `READCARD` | module golden set (SYSOUT) | verified |
| CBACT03C | READXREF | batch-service `READXREF` | module golden set (SYSOUT) | verified |
| CBCUS01C | READCUST | batch-service `READCUST` | module golden set (SYSOUT) | verified |
| COBSWAIT | WAITSTEP | batch-service `WAITSTEP` (`Thread.sleep` for `MVSWAIT`) | `WaitStepJobConfigTest` | verified (job test); not needed in the schedules |
| CSUTLDTC | called by COTRN02C, CORPT00C, CSUTLDPY | `common` `DateValidation`; stopgaps `CsutldtcDate` (transaction-service), `ReportDates` (batch-service) | `DateValidationTest` and the COTRN02C/CORPT00C flow tests | partial: no LE runtime, CEEDAYS conditions are a hypothesis |

### 2.3 Not migrated (outside `app/cbl`)

| Item | Why |
|---|---|
| Assembler `COBDATFT` (+ `COCDATFT.mac`) | Replaced by `java.time` in READACCT; checked through CBACT01C's golden outputs. |
| Assembler `MVSWAIT` (+ `ASMWAIT.mac`) | Replaced by `Thread.sleep` in WAITSTEP. |
| `app/app-authorization-ims-db2-mq`, `app/app-transaction-type-db2`, `app/app-vsam-mq` | Optional IMS/DB2/MQ extension modules, out of scope (d-scope). CA-7's `CBPAUP0J` is dropped from the job order. |
| Control-M (`CardDemo.controlm`), CA-7 (`CardDemo.ca7`) | Replaced by the explicit Java job order ([jcl-mapping.md](jcl-mapping.md) "Schedule"); calendars and conditions not ported. |
| 3270/BMS presentation (`app/bms`, attribute bytes, cursor positioning, PF3/PF4/PF12) | Replaced by the REST API; navigation and clear keys are client-side. |

## 3. Online flow tests per program (generated)

<!-- BEGIN GENERATED: online flow tests (scripts/migration/coverage-matrix.py) -->

74 `@CobolFlow` tests over 17 programs, read from the test sources (`modernization/*/src/test/java/**/flow/`). Do not edit by hand.

| Program | Module | Test class | Paragraphs (screen keys) | Tests |
|---|---|---|---|---|
| COACTUPC | `account-service` | `AccountFlowTest`, `AccountUpdateMessagesFlowTest` | 9000-READ-ACCT (ENTER)<br>1210-EDIT-ACCOUNT (PF5)<br>1205-COMPARE-OLD-NEW (PF5)<br>9600-WRITE-PROCESSING (PF5)<br>9700-CHECK-CHANGE-IN-REC (PF5)<br>1200-EDIT-MAP-INPUTS (PF5)<br>1225-EDIT-ALPHA-REQD (PF5) | 7 |
| COACTVWC | `account-service` | `AccountFlowTest` | 9000-READ-ACCT (ENTER)<br>2210-EDIT-ACCOUNT (ENTER)<br>9200-GETCARDXREF-BYACCT (ENTER) | 3 |
| COADM01C | `auth-service` | `SignonAndMenuFlowTest` | BUILD-MENU-OPTIONS (ENTER)<br>PROCESS-ENTER-KEY (ENTER)<br>PGMIDERR-ERR-PARA (ENTER) | 4 |
| COBIL00C | `transaction-service` | `TransactionFlowTest` | UPDATE-ACCTDAT-FILE (ENTER)<br>PROCESS-ENTER-KEY (ENTER)<br>READ-ACCTDAT-FILE (ENTER) | 3 |
| COCRDLIC | `card-service` | `CardFlowTest` | 9000-READ-FORWARD (ENTER, PF8)<br>9100-READ-BACKWARDS (PF7)<br>9500-FILTER-RECORDS (ENTER)<br>2210-EDIT-ACCOUNT (ENTER)<br>2220-EDIT-CARD (ENTER) | 8 |
| COCRDSLC | `card-service` | `CardFlowTest` | 9100-GETCARD-BYACCTCARD (ENTER)<br>2220-EDIT-CARD (ENTER) | 3 |
| COCRDUPC | `card-service` | `CardFlowTest` | 9100-GETCARD-BYACCTCARD (ENTER, PF5)<br>1200-EDIT-MAP-INPUTS (PF5)<br>9200-WRITE-PROCESSING (PF5)<br>9300-CHECK-CHANGE-IN-REC (PF5)<br>1230-EDIT-NAME (PF5) | 7 |
| COMEN01C | `auth-service` | `SignonAndMenuFlowTest` | BUILD-MENU-OPTIONS (ENTER)<br>PROCESS-ENTER-KEY (ENTER)<br>MAIN-PARA (ENTER) | 6 |
| CORPT00C | `batch-service` | `ReportSubmissionFlowTest` | SUBMIT-JOB-TO-INTRDR (ENTER)<br>PROCESS-ENTER-KEY (ENTER) | 2 |
| COSGN00C | `auth-service` | `SignonAndMenuFlowTest` | PROCESS-ENTER-KEY (ENTER)<br>READ-USER-SEC-FILE (ENTER) | 6 |
| COTRN00C | `transaction-service` | `TransactionFlowTest` | PROCESS-PAGE-FORWARD (ENTER)<br>STARTBR-TRANSACT-FILE (ENTER)<br>PROCESS-PF8-KEY (PF8)<br>PROCESS-PF7-KEY (PF7) | 4 |
| COTRN01C | `transaction-service` | `TransactionFlowTest` | READ-TRANSACT-FILE (ENTER) | 2 |
| COTRN02C | `transaction-service` | `TransactionFlowTest` | ADD-TRANSACTION (ENTER)<br>READ-CCXREF-FILE (ENTER)<br>READ-CXACAIX-FILE (ENTER)<br>VALIDATE-INPUT-KEY-FIELDS (ENTER)<br>VALIDATE-INPUT-DATA-FIELDS (ENTER)<br>COPY-LAST-TRAN-DATA (PF5) | 6 |
| COUSR00C | `auth-service` | `UserAdministrationFlowTest` | PROCESS-PAGE-FORWARD (ENTER)<br>STARTBR-USER-SEC-FILE (ENTER)<br>PROCESS-PF8-KEY (PF8)<br>PROCESS-PF7-KEY (PF7) | 4 |
| COUSR01C | `auth-service` | `UserAdministrationFlowTest` | PROCESS-ENTER-KEY (ENTER)<br>WRITE-USER-SEC-FILE (ENTER) | 3 |
| COUSR02C | `auth-service` | `UserAdministrationFlowTest` | READ-USER-SEC-FILE (ENTER)<br>UPDATE-USER-INFO (PF5)<br>UPDATE-USER-SEC-FILE (PF5) | 4 |
| COUSR03C | `auth-service` | `UserAdministrationFlowTest` | READ-USER-SEC-FILE (ENTER)<br>DELETE-USER-SEC-FILE (PF5) | 2 |

<details><summary>Test methods</summary>

- **COACTUPC**: `COACTUPC_9000_READ_ACCT_enterFetchesTheRecordsToUpdate`, `COACTUPC_1210_EDIT_ACCOUNT_zeroAccountNumberIsRejected`, `COACTUPC_1205_COMPARE_OLD_NEW_unchangedScreenIsNotWritten`, `COACTUPC_9600_WRITE_PROCESSING_validChangesRewriteAccountAndCustomer`, `COACTUPC_9700_CHECK_CHANGE_IN_REC_recordChangedSinceFetchIsRefused`, `COACTUPC_1200_EDIT_MAP_INPUTS_everyEditMessage`, `COACTUPC_1225_EDIT_ALPHA_REQD_storedCountryIsEditedToo`
- **COACTVWC**: `COACTVWC_9000_READ_ACCT_knownAccountShowsAccountAndCustomer`, `COACTVWC_2210_EDIT_ACCOUNT_zeroOrNonNumericAccountIsRejected`, `COACTVWC_9200_GETCARDXREF_BYACCT_unknownAccountIsNotFound`
- **COADM01C**: `COADM01C_BUILD_MENU_OPTIONS_adminSeesTheSixAdminMenuOptions`, `COADM01C_PROCESS_ENTER_KEY_validOptionNamesTheProgramToTransferTo`, `COADM01C_PROCESS_ENTER_KEY_optionOutOfRangeIsRejected`, `COADM01C_PGMIDERR_ERR_PARA_optionNotInstalledIsReported`
- **COBIL00C**: `COBIL00C_UPDATE_ACCTDAT_FILE_positiveBalanceIsPaidThenNothingIsLeftToPay`, `COBIL00C_PROCESS_ENTER_KEY_zeroOrNegativeBalanceHasNothingToPay`, `COBIL00C_READ_ACCTDAT_FILE_unknownAccountIsNotFound`
- **COCRDLIC**: `COCRDLIC_9000_READ_FORWARD_firstScreenShowsSevenCardsInCardNumberOrder`, `COCRDLIC_9000_READ_FORWARD_pf8ShowsTheNextSevenAndTheLastScreenHasOne`, `COCRDLIC_9100_READ_BACKWARDS_pf7FromTheSecondScreenShowsTheFirstAgain`, `COCRDLIC_9500_FILTER_RECORDS_accountFilterShowsThatAccountsCards`, `COCRDLIC_9500_FILTER_RECORDS_cardFilterAndBothFiltersIntersect`, `COCRDLIC_2210_EDIT_ACCOUNT_nonNumericAccountFilterIsRejected`, `COCRDLIC_2220_EDIT_CARD_nonNumericCardFilterIsRejected`, `COCRDLIC_9000_READ_FORWARD_userAndAdminSeeTheSameCards`
- **COCRDSLC**: `COCRDSLC_9100_GETCARD_BYACCTCARD_knownCardIsShown`, `COCRDSLC_9100_GETCARD_BYACCTCARD_unknownCardIsNotFound`, `COCRDSLC_2220_EDIT_CARD_zeroAndNonNumericCardNumbersAreRejected`
- **COCRDUPC**: `COCRDUPC_9100_GETCARD_BYACCTCARD_enterFetchesTheCardToUpdate`, `COCRDUPC_1200_EDIT_MAP_INPUTS_everyEditMessage`, `COCRDUPC_1200_EDIT_MAP_INPUTS_unchangedCardIsNotRewritten`, `COCRDUPC_9200_WRITE_PROCESSING_changedCardIsRewrittenKeepingTheDay`, `COCRDUPC_9300_CHECK_CHANGE_IN_REC_cardChangedSinceFetchIsRefused`, `COCRDUPC_9100_GETCARD_BYACCTCARD_unknownCardIsNotFound`, `COCRDUPC_1230_EDIT_NAME_apostropheInTheSampleNameBlocksAStatusChange`
- **COMEN01C**: `COMEN01C_BUILD_MENU_OPTIONS_userSeesTheElevenMainMenuOptions`, `COMEN01C_PROCESS_ENTER_KEY_validOptionNamesTheProgramToTransferTo`, `COMEN01C_PROCESS_ENTER_KEY_optionOutOfRangeIsRejected`, `COMEN01C_PROCESS_ENTER_KEY_optionNotInstalledIsReported`, `COMEN01C_PROCESS_ENTER_KEY_adminOnlyFunctionIsRefusedToAUser`, `COMEN01C_MAIN_PARA_menuWithoutSignOnIsRefused`
- **CORPT00C**: `CORPT00C_SUBMIT_JOB_TO_INTRDR_validCustomRangeProducesTheReport`, `CORPT00C_PROCESS_ENTER_KEY_invalidDateRangesAreRefusedWithTheCobolMessage`
- **COSGN00C**: `COSGN00C_PROCESS_ENTER_KEY_blankUserIdAsksForTheUserId`, `COSGN00C_PROCESS_ENTER_KEY_blankPasswordAsksForThePassword`, `COSGN00C_READ_USER_SEC_FILE_unknownUserIsNotFound`, `COSGN00C_READ_USER_SEC_FILE_wrongPasswordIsRefused`, `COSGN00C_READ_USER_SEC_FILE_adminIsRoutedToTheAdminMenu`, `COSGN00C_READ_USER_SEC_FILE_regularUserIsRoutedToTheMainMenu`
- **COTRN00C**: `COTRN00C_PROCESS_PAGE_FORWARD_firstScreenListsTenTransactionsInIdOrder`, `COTRN00C_STARTBR_TRANSACT_FILE_typedTranIdStartsTheBrowse`, `COTRN00C_PROCESS_PF8_KEY_nextScreenStartsAfterTheLastRowShown`, `COTRN00C_PROCESS_PF7_KEY_previousScreenAndNothingBeforeTheTop`
- **COTRN01C**: `COTRN01C_READ_TRANSACT_FILE_knownTransactionIsShown`, `COTRN01C_READ_TRANSACT_FILE_unknownTransactionIsNotFound`
- **COTRN02C**: `COTRN02C_ADD_TRANSACTION_addByAccountThenViewIt`, `COTRN02C_READ_CCXREF_FILE_addByCardNumber`, `COTRN02C_READ_CXACAIX_FILE_unknownAccountIsNotFound`, `COTRN02C_VALIDATE_INPUT_KEY_FIELDS_everyKeyFieldMessage`, `COTRN02C_VALIDATE_INPUT_DATA_FIELDS_everyDataFieldMessage`, `COTRN02C_COPY_LAST_TRAN_DATA_lastRecordIsTheLastListRow`
- **COUSR00C**: `COUSR00C_PROCESS_PAGE_FORWARD_firstScreenListsTenUsersInIdOrder`, `COUSR00C_STARTBR_USER_SEC_FILE_searchUserIdStartsTheBrowse`, `COUSR00C_PROCESS_PF8_KEY_nextScreenShowsTheRemainingUsers`, `COUSR00C_PROCESS_PF7_KEY_previousScreenAndNothingBeforeTheTop`
- **COUSR01C**: `COUSR01C_PROCESS_ENTER_KEY_eachEmptyFieldInScreenOrder`, `COUSR01C_WRITE_USER_SEC_FILE_duplicateUserIdIsRefused`, `COUSR01C_WRITE_USER_SEC_FILE_addedUserCanSignOn`
- **COUSR02C**: `COUSR02C_READ_USER_SEC_FILE_enterFetchesTheUser`, `COUSR02C_UPDATE_USER_INFO_eachEmptyFieldInScreenOrder`, `COUSR02C_UPDATE_USER_INFO_unchangedUserAsksForAModification`, `COUSR02C_UPDATE_USER_SEC_FILE_changedFieldsAreRewritten`
- **COUSR03C**: `COUSR03C_READ_USER_SEC_FILE_enterFetchesTheUserToDelete`, `COUSR03C_DELETE_USER_SEC_FILE_deletedUserIsGoneAndCannotSignOn`

</details>

<!-- END GENERATED: online flow tests -->

## 4. JCL jobs (38) and procedures (2)

### 4.1 Business batch (14)

| JCL | Java job | Verification | Status |
|---|---|---|---|
| POSTTRAN | transaction-service `POSTTRAN` | golden-set chain | verified |
| INTCALC | transaction-service `INTCALC` | golden-set chain; `IntcalcCobolParityTest` | verified |
| COMBTRAN | transaction-service `COMBTRAN` (sort; the REPRO step is not needed) | golden-set chain (Java output vs the DFSORT step emulated in `verification/reference.py`); `BatchJobsTest` | verified; TRANSACT.BKUP(0) comes from `--dd` or INTCALC's generation until TRANBKP exists |
| TRANREPT | batch-service `TRANREPT` | golden-set chain; 11-case module golden set | verified |
| CREASTMT | batch-service `CREASTMT` | golden-set chain; module golden set | verified |
| CBEXPORT | batch-service `CBEXPORT` | module golden set | verified |
| CBIMPORT | batch-service `CBIMPORT` | module golden set | verified |
| READACCT | batch-service `READACCT` | module golden set | verified |
| READCARD | batch-service `READCARD` | module golden set | verified |
| READCUST | batch-service `READCUST` | module golden set | verified |
| READXREF | batch-service `READXREF` | module golden set | verified |
| PRTCATBL | none (would be a `RecordSort` over TCATBALF) | — | planned (no ticket) |
| TRANBKP | none (would be a `RecordWriter` unload of `transaction.transactions`) | — | planned (no ticket) |
| TXT2PDF1 | none: PDF rendering; STMTFILE/HTMLFILE are migrated | — | not migrated |

### 4.2 Data-definition only (17): replaced by Flyway + `data-loader`

| JCL | Replaced by | Status |
|---|---|---|
| ACCTFILE, CARDFILE, XREFFILE, CUSTFILE, TCATBALF, DISCGRP, TRANTYPE, TRANCATG | service Flyway schema; data-loader loads the matching `app/data/ASCII` file | data-definition; reconciled (`SampleDataReconciliationTest`, 0 field differences) |
| DUSRSECJ | auth-service Flyway; data-loader loads EBCDIC `USRSEC.PS` (passwords BCrypt-hashed) | data-definition; reconciled |
| TRANFILE, TRANIDX | transaction-service Flyway (`transactions`, indexes); DALYTRAN.PS.INIT not loaded (empty seed) | data-definition |
| ESDSRRDS, DEFCUST | none needed (demonstration / unused cluster) | not migrated |
| DEFGDGB, DEFGDGD, DALYREJS, REPTFILE | `JobRunDirectory` generation directories `<out>/<JOB>/<ts>/<DD>`; reference data by data-loader | data-definition |

### 4.3 Operational and utility (7)

| JCL | Java | Status |
|---|---|---|
| CLOSEFIL, OPENFIL | none: online and batch share one database, no CICS files to close | not migrated |
| WAITSTEP | batch-service `WAITSTEP` (`WaitStepJobConfigTest`) | verified (job test); dropped from the schedules |
| CBADMCDJ | none: DFHCSDUP, CICS-only, names differ from `CARDDEMO.CSD` | not migrated |
| FTPJCL | none: file transfer is outside the services | not migrated |
| INTRDRJ1, INTRDRJ2 | none: internal-reader demo; CORPT00C's internal-reader submit is the async report queue | not migrated |

Count: 14 + 17 + 7 = 38.

### 4.4 Procedures (2)

| Proc | Java | Status |
|---|---|---|
| REPROC.prc | `RecordWriter` table unload or a `--dd` file (no separate proc) | replaced |
| TRANREPT.prc | batch-service `TRANREPT`; `TranreptJobConfig.arguments(start, end)` = `EXEC PROC=TRANREPT` from CORPT00C | verified with TRANREPT |
