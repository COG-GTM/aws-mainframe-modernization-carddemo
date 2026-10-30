# Business rules catalog - AUTHFRDS (fraud-authorization records)

> **Provenance:** authored by **Devin** (Cognition) in the AWS Transform business-logic layout. **Not produced by AWS Transform.** Source of truth: the COBOL/DDL under `app/app-authorization-ims-db2-mq/`. Real AWS Transform output (if any) lives only under `aws-transform-output/`.

Module: `app/app-authorization-ims-db2-mq/`. Programs in scope: `COPAUS2C.cbl` (Db2 owner of `CARDDEMO.AUTHFRDS`) and the PF5/PF8/ENTER paths of `COPAUS1C.cbl` that call it. IMS (DBPAUTP0), CICS BMS and MQ are modelled as inputs/outputs only.

| ID | Rule | COBOL source |
|----|------|--------------|
| BR-AF-001 | `AUTH-TS` (X(26)) = `YY-MM-DD` from `PA-AUTH-ORIG-DATE` (YYMMDD) + ` ` + time `HH.MI.SSsss` built from `WS-AUTH-TIME = 999999999 - PA-AUTH-TIME-9C` + `000`; inserted with `TIMESTAMP_FORMAT(:AUTH-TS, 'YY-MM-DD HH24.MI.SSNNNNNN')`. Two-digit year: on Db2 LUW `99-..` resolves to 2099 (observed), so the century comes from Db2, not the program. | COPAUS2C `MAIN-PARA` (COMPUTE line 107-111, INSERT VALUES) |
| BR-AF-002 | Every CIPAUDTY field is MOVEd to its DCLGEN host variable; `AUTH_FRAUD` = the commarea flag, `ACCT_ID`/`CUST_ID` from the commarea `WS-ACCT-ID`/`WS-CUST-ID` (COPAUS1C fills them as `WS-FRD-ACCT-ID`/`WS-FRD-CUST-ID`); `MERCHANT-NAME-LEN = LENGTH OF PA-MERCHANT-NAME` (always 22, blank padded). No null indicators. | COPAUS2C `MAIN-PARA` (MOVEs, line 130) |
| BR-AF-003 | `FRAUD_RPT_DATE` is Db2 `CURRENT DATE` on INSERT and on UPDATE (the commarea date is ignored). | COPAUS2C `MAIN-PARA` INSERT, `FRAUD-UPDATE` |
| BR-AF-004 | INSERT SQLCODE 0 -> `WS-FRD-UPDT-SUCCESS`, message `ADD SUCCESS`. | COPAUS2C `MAIN-PARA` |
| BR-AF-005 | INSERT SQLCODE -803 (duplicate key CARD_NUM+AUTH_TS) -> `PERFORM FRAUD-UPDATE`: UPDATE only `AUTH_FRAUD` and `FRAUD_RPT_DATE` for that key; all other columns keep their first values. | COPAUS2C `MAIN-PARA`, `FRAUD-UPDATE` |
| BR-AF-006 | UPDATE SQLCODE 0 -> success, message `UPDT SUCCESS`. | COPAUS2C `FRAUD-UPDATE` |
| BR-AF-007 | UPDATE SQLCODE != 0 (incl. +100) -> `WS-FRD-UPDT-FAILED`, message ` UPDT ERROR DB2: CODE:` + WS-SQLCODE (PIC +9(06)) + `, STATE: ` + WS-SQLSTATE (PIC +9(09)). | COPAUS2C `FRAUD-UPDATE` |
| BR-AF-008 | INSERT SQLCODE other than 0/-803 -> failed, message ` SYSTEM ERROR DB2: CODE:` + WS-SQLCODE + `, STATE: ` + WS-SQLSTATE. | COPAUS2C `MAIN-PARA` |
| BR-AF-009 | `WS-FRD-ACT-MSG` is PIC X(50); STRING stops at 50 characters. | COPAUS2C WORKING-STORAGE / LINKAGE `WS-FRD-ACT-MSG` |
| BR-AF-010 | `EXEC CICS ASKTIME` + `FORMATTIME MMDDYY DATESEP` -> `PA-FRAUD-RPT-DATE` (`MM/DD/YY`) returned in the commarea (used by COPAUS1C for the IMS REPL). | COPAUS2C `MAIN-PARA` lines 91-101 |
| BR-AF-011 | The commarea layout bounds every input (PIC sizes of CIPAUDTY + `WS-ACCT-ID` 9(11), `WS-CUST-ID` 9(9), `WS-FRD-ACTION` 'F'/'R'). Java rejects out-of-PIC input before SQL. | COPAUS2C LINKAGE, `cpy/CIPAUDTY.cpy` |
| BR-AF-020 | PF5: if `PA-FRAUD-CONFIRMED` ('F') -> `PA-FRAUD-REMOVED` ('R') + `WS-REMOVE-FRAUD`; else -> 'F' + `WS-REPORT-FRAUD`. | COPAUS1C `MARK-AUTH-FRAUD` |
| BR-AF-021 | `WS-FRAUD-DATA` = CDEMO-ACCT-ID, CDEMO-CUST-ID, the detail with the toggled flag; `EXEC CICS LINK PROGRAM('COPAUS2C')`. | COPAUS1C `MARK-AUTH-FRAUD` |
| BR-AF-022 | COPAUS2C success -> `UPDATE-AUTH-DETAILS` (REPL PAUTDTL1 with the returned record) -> `TAKE-SYNCPOINT`; message `AUTH MARKED FRAUD...` / `AUTH FRAUD REMOVED...`. | COPAUS1C `MARK-AUTH-FRAUD`, `UPDATE-AUTH-DETAILS` |
| BR-AF-023 | COPAUS2C failure -> `WS-MESSAGE = WS-FRD-ACT-MSG`, `ROLL-BACK` (no IMS update). | COPAUS1C `MARK-AUTH-FRAUD`, `ROLL-BACK` |
| BR-AF-024 | IMS REPL failure -> `ROLL-BACK` (undoes the Db2 write in the same UOW), message ` System error while FRAUD Tagging, ROLLBACK||` + IMS-RETURN-CODE. | COPAUS1C `UPDATE-AUTH-DETAILS` |
| BR-AF-025 | `READ-AUTH-RECORD`: GU PAUTSUM0 by ACCNTID, GNP PAUTDTL1 by PAUT9CTS; GE/GB -> AUTHS-EOF; other status -> ` System error while reading Auth Summary/Details: Code:` + status. | COPAUS1C `READ-AUTH-RECORD` |
| BR-AF-026 | The selected authorization key (`CDEMO-CPVD-PAU-SELECTED`) must be present (not SPACES/LOW-VALUES) to read. | COPAUS1C `PROCESS-ENTER-KEY` |
| BR-AF-027 | PF8: read current then GNP next PAUTDTL1; at end `Already at the last Authorization...`; other status ` System error while reading next Auth: Code:`. | COPAUS1C `PROCESS-PF8-KEY`, `READ-NEXT-AUTH-RECORD` |
| BR-AF-030 | *Java-only inspection read*: list AUTHFRDS in XAUTHFRD order (CARD_NUM ASC, AUTH_TS DESC), paged, filterable. No legacy program reads AUTHFRDS. | `ddl/XAUTHFRD.ddl` (index order only) |
| BR-AF-040 | *Java-only access control* (legacy: CICS signon COSGN00C / RACF upstream of COPAUS1C): stateless HTTP Basic. `GET /api/**` needs READER or MAINTAINER, any write needs MAINTAINER; `/actuator/health` and Swagger are public. Credentials come only from `AUTHFRDS_API_USER`/`_PASSWORD` and `AUTHFRDS_API_READER_USER`/`_PASSWORD`; startup fails if any is unset or the two usernames match (the unit-test `test` profile is exempt). | COSGN00C (outside this module), `csd/CRDDEMO2.csd` transaction definitions |
| BR-AF-041 | *Java-only*: the IMS stub test hooks (`/api/v1/ims-stub/*`) are only registered under the `testing` Spring profile; without it they return 404. | n/a (stub control, no legacy equivalent) |
