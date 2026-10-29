# Data model - CARDDEMO.AUTHFRDS

> **Provenance:** authored by **Devin** (Cognition) in the AWS Transform business-logic layout. **Not produced by AWS Transform.** Source of truth: the COBOL/DDL under `app/app-authorization-ims-db2-mq/`. Real AWS Transform output (if any) lives only under `aws-transform-output/`.

## Db2 table (`ddl/AUTHFRDS.ddl`)
26 columns; PK `(CARD_NUM, AUTH_TS)`; unique index `XAUTHFRD (CARD_NUM ASC, AUTH_TS DESC)` (`ddl/XAUTHFRD.ddl`). No foreign keys (so no SQLCODE -532 path).

| Column | Type | DCLGEN host var (`dcl/AUTHFRDS.dcl`) | Source (CIPAUDTY / commarea) |
|---|---|---|---|
| CARD_NUM | CHAR(16) NOT NULL | CARD-NUM | PA-CARD-NUM |
| AUTH_TS | TIMESTAMP NOT NULL | AUTH-TS X(26) | BR-AF-001 |
| AUTH_TYPE | CHAR(4) | AUTH-TYPE | PA-AUTH-TYPE |
| CARD_EXPIRY_DATE | CHAR(4) | CARD-EXPIRY-DATE | PA-CARD-EXPIRY-DATE |
| MESSAGE_TYPE / MESSAGE_SOURCE | CHAR(6) | MESSAGE-TYPE / -SOURCE | PA-MESSAGE-TYPE / -SOURCE |
| AUTH_ID_CODE | CHAR(6) | AUTH-ID-CODE | PA-AUTH-ID-CODE |
| AUTH_RESP_CODE / AUTH_RESP_REASON | CHAR(2) / CHAR(4) | | PA-AUTH-RESP-CODE / -REASON |
| PROCESSING_CODE | CHAR(6) | | PA-PROCESSING-CODE 9(6) |
| TRANSACTION_AMT / APPROVED_AMT | DECIMAL(12,2) | S9(10)V99 COMP-3 | PA-TRANSACTION-AMT / PA-APPROVED-AMT |
| MERCHANT_CATAGORY_CODE (sic) | CHAR(4) | | PA-MERCHANT-CATAGORY-CODE |
| ACQR_COUNTRY_CODE | CHAR(3) | | PA-ACQR-COUNTRY-CODE |
| POS_ENTRY_MODE | SMALLINT | S9(4) COMP | PA-POS-ENTRY-MODE 9(2) |
| MERCHANT_ID | CHAR(15) | | PA-MERCHANT-ID |
| MERCHANT_NAME | VARCHAR(22) | MERCHANT-NAME-LEN/-TEXT | PA-MERCHANT-NAME (len always 22, BR-AF-002) |
| MERCHANT_CITY / STATE / ZIP | CHAR(13)/(2)/(9) | | PA-MERCHANT-* |
| TRANSACTION_ID | CHAR(15) | | PA-TRANSACTION-ID |
| MATCH_STATUS | CHAR(1) | | PA-MATCH-STATUS (P/D/E/M) |
| AUTH_FRAUD | CHAR(1) | | WS-FRD-ACTION / PA-AUTH-FRAUD ('F' confirmed, 'R' removed) |
| FRAUD_RPT_DATE | DATE | | Db2 CURRENT DATE (BR-AF-003) |
| ACCT_ID | DECIMAL(11) | | commarea WS-ACCT-ID |
| CUST_ID | DECIMAL(9) | | commarea WS-CUST-ID |

## Seed data
There are **no Db2 load cards** for AUTHFRDS in the repo (`find app/app-authorization-ims-db2-mq -iname '*load*'` shows none for this table): the table starts empty and is populated only by COPAUS2C. Observed after init: `SELECT COUNT(*) FROM CARDDEMO.AUTHFRDS` = 0.

The pending authorizations the fraud flow acts on come from IMS (`data/EBCDIC/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat`, 51,736 bytes). `modernization/authfrds-service/tools/decode_ims_unload.py` decodes it (cp037 + COMP-3) into 22 PAUTSUM0 roots and 202 PAUTDTL1 children (observed run), which seed the IMS **stub**.

## IMS segments (inputs/outputs, stubbed)
- `PAUTSUM0` (CIPAUSMY, key ACCNTID 9(11) COMP-3) - root, one per account.
- `PAUTDTL1` (CIPAUDTY, key PAUT9CTS = PA-AUTH-DATE-9C 9(5) COMP-3 + PA-AUTH-TIME-9C 9(9) COMP-3) - child. Fields `PA-AUTH-FRAUD` and `PA-FRAUD-RPT-DATE` X(8) are REPLaced after PF5.

## MQ
Not used by COPAUS2C or the COPAUS1C fraud path (MQ belongs to COPAUA0C authorization processing) - out of scope.
