# COPAUS2C - Mark / unmark an authorization as fraud in Db2

> **Provenance:** authored by **Devin** (Cognition) in the AWS Transform business-logic layout. **Not produced by AWS Transform.** Source of truth: the COBOL/DDL under `app/app-authorization-ims-db2-mq/`. Real AWS Transform output (if any) lives only under `aws-transform-output/`.

## Overview
CICS program (no BMS map) LINKed from COPAUS1C. Writes one row of `CARDDEMO.AUTHFRDS` per (card, authorization timestamp): INSERT first time, UPDATE the fraud flag thereafter. Rules BR-AF-001..011.

## Inputs / outputs (DFHCOMMAREA `WS-FRAUD-DATA`)
| Field | PIC | Dir |
|---|---|---|
| WS-ACCT-ID | 9(11) | in |
| WS-CUST-ID | 9(9) | in |
| WS-FRAUD-AUTH-RECORD | CIPAUDTY (PAUTDTL1 image) | in; PA-FRAUD-RPT-DATE out |
| WS-FRD-ACTION | X ('F' report / 'R' remove) | in |
| WS-FRD-UPDATE-STATUS | X ('S' success / 'F' failed) | out |
| WS-FRD-ACT-MSG | X(50) | out |

## Flow (`MAIN-PARA`)
1. ASKTIME / FORMATTIME MMDDYY DATESEP -> PA-FRAUD-RPT-DATE (BR-AF-010).
2. Build AUTH-TS (BR-AF-001); MOVE all fields to DCLAUTHFRDS (BR-AF-002).
3. INSERT (BR-AF-003). SQLCODE 0 -> `ADD SUCCESS` (BR-AF-004); -803 -> `FRAUD-UPDATE` (BR-AF-005); else ` SYSTEM ERROR DB2: ...` (BR-AF-008).
4. `FRAUD-UPDATE`: UPDATE AUTH_FRAUD, FRAUD_RPT_DATE = CURRENT DATE WHERE CARD_NUM AND AUTH_TS; 0 -> `UPDT SUCCESS` (BR-AF-006) else ` UPDT ERROR DB2: ...` (BR-AF-007).
5. EXEC CICS RETURN. No SYNCPOINT here - the caller owns the unit of work (BR-AF-022..024).

## Entry points
Only `EXEC CICS LINK PROGRAM('COPAUS2C')` from COPAUS1C. No PF keys.

## SQL
```sql
INSERT INTO CARDDEMO.AUTHFRDS (CARD_NUM, AUTH_TS, ..., AUTH_FRAUD, FRAUD_RPT_DATE, ACCT_ID, CUST_ID)
VALUES (:CARD-NUM, TIMESTAMP_FORMAT(:AUTH-TS, 'YY-MM-DD HH24.MI.SSNNNNNN'), ..., :AUTH-FRAUD, CURRENT DATE, :ACCT-ID, :CUST-ID);
UPDATE CARDDEMO.AUTHFRDS SET AUTH_FRAUD = :AUTH-FRAUD, FRAUD_RPT_DATE = CURRENT DATE
 WHERE CARD_NUM = :CARD-NUM AND AUTH_TS = TIMESTAMP_FORMAT(:AUTH-TS, 'YY-MM-DD HH24.MI.SSNNNNNN');
```
