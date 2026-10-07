# COCRDSLC — Credit card detail view (transaction CCDL, map CCRDSLA / mapset COCRDSL)

Source: `app/cbl/COCRDSLC.cbl` (structured-paragraph style with `CSSTRPFY` PF-key mapping). Data access: `CARDDAT` KSDS
`READ` by `WS-CARD-RID-CARDNUM X(16)` (record `CVACT02Y`); an unused `9150-GETCARD-BYACCT` path reads `CARDAIX` by account.
Commarea: `CARDDEMO-COMMAREA` (`CDEMO-ACCT-ID 9(11)`, `CDEMO-CARD-NUM 9(16)`, `CDEMO-LAST-MAP/MAPSET`) followed by
`WS-THIS-PROGCOMMAREA` (`CA-FROM-PROGRAM`, `CA-FROM-TRANID`); returned as `WS-COMMAREA X(2000)`.
Literals: `LIT-MENUPGM='COMEN01C'`, `LIT-MENUTRANID='CM00'`, `LIT-CCLISTPGM='COCRDLIC'`, `LIT-CCLISTMAPSET='COCRDLI'`.

## 0000-MAIN

| # | Given | Then |
|---|---|---|
| R-1 | Any entry | `HANDLE ABEND LABEL(ABEND-ROUTINE)`; work areas initialised; `WS-RETURN-MSG` blank. |
| R-2 | `EIBCALEN = 0`, or `CDEMO-FROM-PROGRAM='COMEN01C'` and `CDEMO-PGM-ENTER` | Commarea initialised (fresh search). |
| R-3 | AID mapping (`YYYY-STORE-PFKEY`) | Only ENTER and PF3 are valid; any other AID is treated as ENTER. |
| R-4 | PF3 | `CDEMO-TO-TRANID ← CDEMO-FROM-TRANID` (blank → `CM00`), `CDEMO-TO-PROGRAM ← CDEMO-FROM-PROGRAM` (blank → `COMEN01C`); from-fields ← `CCDL`/`COCRDSLC`; `CDEMO-USRTYP-USER` set (**user type forced to 'U'**); `CDEMO-PGM-ENTER`; last map/mapset ← `CCRDSLA`/`COCRDSL`; `XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)`. |
| R-5 | `CDEMO-PGM-ENTER` and `CDEMO-FROM-PROGRAM='COCRDLIC'` | Selected card from the list: `CC-ACCT-ID-N ← CDEMO-ACCT-ID`, `CC-CARD-NUM-N ← CDEMO-CARD-NUM`; `9000-READ-DATA`; send map. |
| R-6 | `CDEMO-PGM-ENTER` (from menu) | Send empty search map with prompt (R-13). |
| R-7 | `CDEMO-PGM-REENTER` | `2000-PROCESS-INPUTS`; if `INPUT-ERROR` → send map with message; else `9000-READ-DATA` then send map. |
| R-8 | Other state | `WS-RETURN-MSG='UNEXPECTED DATA SCENARIO'`, `ABEND-CODE='0001'`; `SEND TEXT` of the message and `RETURN` (task ends, no transid). |
| R-9 | `COMMON-RETURN` | `CCARD-ERROR-MSG ← WS-RETURN-MSG`; `RETURN TRANSID('CCDL') COMMAREA(WS-COMMAREA) LENGTH(2000)`. |

## 2200-EDIT-MAP-INPUTS (after `RECEIVE MAP('CCRDSLA') MAPSET('COCRDSL')`)

| # | Given | Then |
|---|---|---|
| R-10 | `ACCTSIDI` = `*` or spaces | `CC-ACCT-ID ← LOW-VALUES`; else the typed value. Same for `CARDSIDI` → `CC-CARD-NUM`. |
| R-11 | `2210-EDIT-ACCOUNT`: account blank/low-values/zero | `INPUT-ERROR`; `FLG-ACCTFILTER-BLANK`; if no message yet, `WS-RETURN-MSG='Account number not provided'`; `CDEMO-ACCT-ID ← 0`. |
| R-12 | Account not numeric | `INPUT-ERROR`; `FLG-ACCTFILTER-NOT-OK`; message (if none yet) `ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER`; `CDEMO-ACCT-ID ← 0`. Otherwise `CDEMO-ACCT-ID ← CC-ACCT-ID`, `FLG-ACCTFILTER-ISVALID`. |
| R-13 | `2220-EDIT-CARD`: card blank/zero | `INPUT-ERROR`; `FLG-CARDFILTER-BLANK`; message (if none yet) `Card number not provided`; `CDEMO-CARD-NUM ← 0`. |
| R-14 | Card not numeric | `INPUT-ERROR`; `FLG-CARDFILTER-NOT-OK`; message (if none yet) `CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER`; `CDEMO-CARD-NUM ← 0`. Otherwise `CDEMO-CARD-NUM ← CC-CARD-NUM-N`, `FLG-CARDFILTER-ISVALID`. |
| R-15 | Both blank | `WS-RETURN-MSG='No input received'` (overrides the account prompt because the 88 is set last). |

Both account **and** card are mandatory (no account-only lookup is exposed, although `9150-GETCARD-BYACCT` exists). The first
message set wins (`IF WS-RETURN-MSG-OFF` guards), so account errors are reported before card errors.

## 9100-GETCARD-BYACCTCARD (`READ FILE('CARDDAT ') RIDFLD(card number)`)

| # | Given | Then |
|---|---|---|
| R-16 | `NORMAL` | `FOUND-CARDS-FOR-ACCOUNT` → `WS-INFO-MSG='   Displaying requested details'`. Note: the account id typed is **not** cross-checked against `CARD-ACCT-ID` of the record read. |
| R-17 | `NOTFND` | `INPUT-ERROR`; both filter flags NOT-OK (fields shown red); message `Did not find cards for this search condition`. |
| R-18 | Other RESP | `INPUT-ERROR`; `FLG-ACCTFILTER-NOT-OK`; message `File Error: READ on CARDDAT  ...` built from `WS-FILE-ERROR-MESSAGE` (`'File Error: '`, op name, `' on '`, file, RESP/RESP2). |

## 1000-SEND-MAP

| # | Given | Then |
|---|---|---|
| R-19 | `1200-SETUP-SCREEN-VARS` | `ACCTSIDO ← CC-ACCT-ID` unless `CDEMO-ACCT-ID = 0` (then low-values); `CARDSIDO ← CC-CARD-NUM` unless `CDEMO-CARD-NUM = 0`. If found: `CRDNAMEO ← CARD-EMBOSSED-NAME`, `EXPMONO ← CARD-EXPIRAION-DATE(6:2)`, `EXPYEARO ← (1:4)`, `CRDSTCDO ← CARD-ACTIVE-STATUS`. If `WS-INFO-MSG` blank → `Please enter Account and Card Number`. `ERRMSGO ← WS-RETURN-MSG`, `INFOMSGO ← WS-INFO-MSG`. |
| R-20 | `1300-SETUP-SCREEN-ATTRS` | Entered from the card list (`CDEMO-LAST-MAPSET='COCRDLI'` and from `COCRDLIC`): account/card fields protected (`DFHBMPRF`, default colour); otherwise unprotected (`DFHBMFSE`). `FLG-*-NOT-OK` → field red; `FLG-*-BLANK` on re-entry → field shows `*` in red. Info message dark (`DFHBMDAR`) when blank, else neutral. |
| R-21 | `1400-SEND-SCREEN` | `CCARD-NEXT-MAP/MAPSET ← CCRDSLA/COCRDSL`; `CDEMO-PGM-REENTER` set; `SEND MAP('CCRDSLA') MAPSET('COCRDSL') CURSOR ERASE FREEKB`. |

## ABEND-ROUTINE

| # | Given | Then |
|---|---|---|
| R-22 | Any abend | `ABEND-MSG` defaults to `UNEXPECTED ABEND OCCURRED.`; `ABEND-CULPRIT='COCRDSLC'`; `SEND` of `ABEND-DATA` with NOHANDLE; `HANDLE ABEND CANCEL`; `ABEND ABCODE('9999')`. |

## Java port notes (UNT51-19, `GET /api/v1/cards/{cardNumber}`, `GET /api/v1/cards/by-account/{accountId}`)

- `9100-GETCARD-BYACCTCARD` = `GET /api/v1/cards/{cardNumber}?accountId=`; `9150-GETCARD-BYACCT` (the CARDAIX
  account path, not reached from `9000-READ-DATA` in the COBOL) = `GET /api/v1/cards/by-account/{accountId}`: the
  account's lowest card number, NOTFND `Did not find this account in cards database`.
- R-16 stays for an ADMIN (typed account not cross-checked). A USER only sees a card of the account given, else
  NOTFND (ADR-0020).
- The full card number is returned, as `CARDSIDO` shows it (ADR-0020).
