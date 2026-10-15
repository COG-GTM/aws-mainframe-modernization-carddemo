# 06 - Traceability matrix

| Legacy | Endpoint / job | Implementation | Parity tests |
|---|---|---|---|
| COSGN00C (CC00) | `POST /api/v1/auth/signon` | identity `SignOnService` | `IdentityParityTest.cosgn00c*` |
| COMEN01C (CM00) / COADM01C (CA00) | `GET /api/v1/menu` | identity `Menus` | `cosgn00cAdminRoutesToAdminMenu`, `cosgn00cRegularUserRoutesToMainMenu`, `adminOptionsAreAdminOnly` |
| COUSR00C (CU00) | `GET /api/v1/users` | `UserAdminService.list` (10 per page) | `cousr00cListsTenPerPageInKeyOrder` |
| COUSR01C (CU01) | `POST /api/v1/users` | `UserAdminService.add` | `cousr01cRequiredFieldsInScreenOrder`, `cousr01cAddDuplicateAndSignOnWithNewUser` |
| COUSR02C (CU02) | `PUT /api/v1/users/{id}` | `UserAdminService.update` | `cousr02cUpdateRequiresAChange` |
| COUSR03C (CU03) | `DELETE /api/v1/users/{id}` | `UserAdminService.delete` | `cousr03cDelete` |
| COACTVWC (CAVW) | `GET /api/v1/accounts/{id}` | account `AccountService.view` | `AccountParityTest.coactvwc*` |
| COACTUPC (CAUP) | `PUT /api/v1/accounts/{id}` | `AccountService.update`, `AccountUpdateValidator` | `coactupcCommitsChangesAndDetectsNoChange`, `coactupcRejectsStaleVersion`, `coactupcFieldEdits` |
| COCRDLIC (CCLI) | `GET /api/v1/cards` | card `CardService.list` (7 per page, CARDAIX filter) | `CardParityTest.cocrdlic*` |
| COCRDSLC (CCDL) | `GET /api/v1/cards/{num}` | `CardService.view` | `cocrdslcViewAndNotFound` |
| COCRDUPC (CCUP) | `PUT /api/v1/cards/{num}` | `CardService.update` | `cocrdupcUpdatesKeepsDayAndDetectsNoChange`, `cocrdupcEdits` |
| COTRN00C (CT00) | `GET /api/v1/transactions` | transaction `TransactionService.list` (10 per page) | `TransactionParityTest.cotrn00cTenRowsPerPageWithPf7Pf8` |
| COTRN01C (CT01) | `GET /api/v1/transactions/{id}` | `TransactionService.view` | `cotrn01cViewAndNotFound` |
| COTRN02C (CT02) | `POST /api/v1/transactions` | `TransactionService.add` | `cotrn02cRequiresConfirmationThenAddsWithNextId`, `cotrn02cFieldEdits` |
| COBIL00C (CB00) | `POST /api/v1/bill-payments` | `BillPaymentService` | `cobil00cPaysFullBalanceAfterConfirmation`, `cobil00cErrors` |
| CORPT00C (CR00) | `POST /api/v1/reports/transactions` | `ReportRequestService` | `corpt00cQueuesCustomReport`, `BatchParityTest.corpt00cRequestIsCompletedByBatch` |
| CBTRN02C (POSTTRAN) | job `POSTTRAN` | batch `PostTransactionsService` | `posttranPostsValidAndRejectsWithLegacyReasonCodes` (100, 102, 103, RC=4), `posttranReasonCode101WhenAccountMissing` |
| CBACT04C (INTCALC) | job `INTCALC` | `InterestCalculationService` | `intcalcAppliesGroupRateDefaultFallbackAndResetsCycle`, `intcalcTruncatesLikeCobolCompute` |
| CBTRN03C (TRANREPT) | job `TRANREPT` | `TransactionReportService` | `tranreptFiltersByProcDateAndTotals`, `tranreptPageTotalEveryTwentyLines` |
| CBSTM03A/B (CREASTMT) | job `CREASTMT` | `StatementService` | `creastmtOneStatementPerXref` |
| DALYTRAN input | `POST /api/v1/batch/daily-transactions` | `DailyTransactionStager` | `batchApiIsAdminOnlyAndRunsJobs` |
| VSAM unloads / copybooks | `data-migration` CLI | `CopybookLayouts`, `CobolCodec`, `VsamToRdsLoader` | `CobolCodecTest`, `LegacyDataParityTest`, `VsamToRdsLoaderTest` |
| COTRTLIC / COTRTUPC (DB2 module) | `GET /api/v1/transaction-types`, `/transaction-categories` (read only) | `TransactionController` | `referenceData` |
| COPAUS0C-2C, CBPAUP0C (IMS/MQ) | phase 2 | - | - |

The parity fixture is `common/src/test/resources/fixtures/carddemo-fixture.sql`: 3 accounts covering an
account-specific rate group, the DEFAULT fallback, ZEROAPR, and an expired near-limit account, plus 12 transactions.
