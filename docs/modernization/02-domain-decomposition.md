# 02 - Domain decomposition

| Service | Port | Legacy programs / transactions | Tables owned | Notes |
|---|---|---|---|---|
| identity-service | 8081 | COSGN00C (CC00), COMEN01C (CM00), COADM01C (CA00), COUSR00C-03C (CU00-CU03) | user_security | Issues JWTs locally. Replaced by Cognito in AWS (see 04) |
| account-service | 8082 | COACTVWC (CAVW), COACTUPC (CAUP) | account, customer (reads card_xref) | Account and customer are updated in one transaction, like CAUP's two REWRITEs |
| card-service | 8083 | COCRDLIC (CCLI), COCRDSLC (CCDL), COCRDUPC (CCUP) | card | Lists 7 cards per page; filters by account using the CARDAIX index |
| transaction-service | 8084 | COTRN00C-02C (CT00-CT02), COBIL00C (CB00), CORPT00C (CR00) | card_transaction, report_request, transaction_type, transaction_category | Lists 10 per page; CR00 queues a request instead of submitting JCL |
| batch-service | 8085 | CBTRN02C (POSTTRAN), CBACT04C (INTCALC), CBTRN03C (TRANREPT), CBSTM03A/B (CREASTMT) | daily_transaction, daily_transaction_reject, tran_cat_balance, disclosure_group | Spring Batch; DALYTRAN is staged via API or S3 |
| common (library) | - | Copybooks CVACT01Y..CSUSR01Y, COCOM01Y | - | JPA entities, Flyway DDL, JWT filter, error model, keyset browse |
| data-migration (CLI) | - | IDCAMS REPRO unloads | all | VSAM to RDS loader with reconciliation |

Not migrated, because they only exist for the mainframe: the CICS/VSAM housekeeping JCL (`CLOSEFIL`, `OPENFIL`,
`DEFGDGB`, the `*FILE.jcl` IDCAMS DEFINE jobs, `ESDSRRDS`, `TRANBKP`, `TRANIDX`), the assembler date utilities
(`app/asm`), and the `CBACT01C`-`CBACT03C`, `CBCUS01C` and `CBTRN01C` file-print utilities. A database replaces
those utilities with SQL queries, and Flyway, Aurora snapshots and the loader cover the housekeeping.

## CICS pattern mapping

| CICS / BMS pattern | REST equivalent |
|---|---|
| `EXEC CICS RETURN TRANSID COMMAREA` (pseudo-conversation) | Each request is self-contained. Identity comes from the JWT (it was in CDEMO-USER-ID / CDEMO-USER-TYPE) |
| `XCTL PROGRAM(...)` menu navigation | `GET /api/v1/menu` returns options with their legacy program and REST path |
| `STARTBR` / `READNEXT` / `READPREV` | `KeysetBrowser` runs keyed `>=` / `<` queries with a limit of page size + 1 |
| `READ ... UPDATE` + `REWRITE` | `@Transactional` + optimistic `@Version` |
| `WRITEQ TD QUEUE('JOBS')` (CORPT00C job submit) | `report_request` row picked up by batch-service |
| PF3 (exit), PF4 (clear) | Client-side navigation. The API has no state to clear |
| `SEND MAP ... ERRMSG` | HTTP 4xx with `ApiError{code, message}`. The message text is kept verbatim |
