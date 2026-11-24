# COPAUS1C - Authorization detail view (fraud paths only)

> **Provenance:** authored by **Devin** (Cognition) in the AWS Transform business-logic layout. **Not produced by AWS Transform.** Source of truth: the COBOL/DDL under `app/app-authorization-ims-db2-mq/`. Real AWS Transform output (if any) lives only under `aws-transform-output/`.

## Overview
CICS/BMS screen COPAU1A (transaction CPVD) showing one pending authorization from IMS. Only the paths that reach AUTHFRDS are in scope; the screen itself is replaced by REST endpoints. IMS DL/I calls are behind `PendingAuthorizationStore` (in-memory **stub**, not IMS).

## Inputs / outputs
- In: CARDDEMO-COMMAREA (`CDEMO-ACCT-ID`, `CDEMO-CUST-ID`, `CDEMO-CPVD-PAU-SELECTED` = PAUT9CTS key), AID key.
- IMS: GU PAUTSUM0, GNP PAUTDTL1, REPL PAUTDTL1 (PSB PSBPAUTB).
- Out: screen fields + `WS-MESSAGE` X(80).

## Flow / PF keys
| Key | Paragraph | Behaviour | Rules |
|---|---|---|---|
| ENTER | PROCESS-ENTER-KEY -> READ-AUTH-RECORD | show the selected authorization | BR-AF-025, BR-AF-026 |
| PF3 | RETURN-TO-PREV-SCREEN | XCTL COPAUS0C | n/a (navigation) |
| PF5 | MARK-AUTH-FRAUD -> LINK COPAUS2C -> UPDATE-AUTH-DETAILS -> TAKE-SYNCPOINT / ROLL-BACK | toggle fraud F/R in Db2 and IMS | BR-AF-020..024 |
| PF8 | PROCESS-PF8-KEY -> READ-NEXT-AUTH-RECORD | next authorization | BR-AF-027 |
| other | MAIN-PARA | `Invalid key pressed. Please see below...` (CSMSG01Y) | n/a in REST (no AID keys) |

## Messages (verbatim)
`AUTH MARKED FRAUD...`, `AUTH FRAUD REMOVED...`, ` System error while FRAUD Tagging, ROLLBACK||`, `Already at the last Authorization...`, ` System error while reading Auth Summary: Code:`, ` System error while reading Auth Details: Code:`, ` System error while reading next Auth: Code:`, ` System error while scheduling PSB: Code:`.
