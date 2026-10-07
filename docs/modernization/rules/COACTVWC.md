# COACTVWC — Account view (transaction CAVW, map CACTVWA / mapset COACTVW)

Source: `app/cbl/COACTVWC.cbl`. Data access (all CICS `READ`, no update):
`CXACAIX` (card-xref AIX by account) by `WS-CARD-RID-ACCT-ID-X X(11)` → `CVACT03Y`;
`ACCTDAT` by account id X(11) → `CVACT01Y`; `CUSTDAT` by `WS-CARD-RID-CUST-ID-X X(9)` → `CVCUS01Y`.
Commarea: `CARDDEMO-COMMAREA` + `WS-THIS-PROGCOMMAREA` (`CA-FROM-PROGRAM`, `CA-FROM-TRANID`), returned as `WS-COMMAREA`.
Input: `ACCTSID` (11). Only ENTER and PF3 are recognised (`CSSTRPFY`); other AIDs are treated as ENTER.

## 0000-MAIN

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0`, or from `COMEN01C` with `CDEMO-PGM-ENTER` | Commarea initialised (fresh screen). |
| R-2 | PF3 | `CDEMO-TO-TRANID/PROGRAM ← CDEMO-FROM-TRANID/PROGRAM` (blank → `CM00`/`COMEN01C`); from-fields ← `CAVW`/`COACTVWC`; `CDEMO-USRTYP-USER` forced; `CDEMO-PGM-ENTER`; last map/mapset ← `CACTVWA`/`COACTVW`; `XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)`. |
| R-3 | `CDEMO-PGM-ENTER` | Send empty map with info `Enter or update id of account to display`. |
| R-4 | `CDEMO-PGM-REENTER` | `RECEIVE MAP('CACTVWA') MAPSET('COACTVW')`; `2200-EDIT-MAP-INPUTS`; `INPUT-ERROR` → send map with message; else `9000-READ-ACCT` then send map. |
| R-5 | Other | `UNEXPECTED DATA SCENARIO` sent as plain text, `ABEND-CODE='0001'`, task ends. |
| R-6 | `COMMON-RETURN` | `CCARD-ERROR-MSG ← WS-RETURN-MSG`; `RETURN TRANSID('CAVW') COMMAREA(WS-COMMAREA)`. |

## 2210-EDIT-ACCOUNT

| # | Given | Then |
|---|---|---|
| R-7 | `ACCTSIDI` = `*` or spaces | `CC-ACCT-ID ← LOW-VALUES`; `INPUT-ERROR`; `FLG-ACCTFILTER-BLANK`; message `Account number not provided`, then overridden by `No input received` (set after); `CDEMO-ACCT-ID ← 0`. On re-display the field shows `*` in red. |
| R-8 | Not numeric or all zeros | `INPUT-ERROR`; `FLG-ACCTFILTER-NOT-OK`; message `Account Filter must  be a non-zero 11 digit number` (two spaces after *must*); `CDEMO-ACCT-ID ← 0`; field red. |
| R-9 | Numeric non-zero | `CDEMO-ACCT-ID ← CC-ACCT-ID`; `FLG-ACCTFILTER-ISVALID`. |

## 9000-READ-ACCT (three reads, stop at first failure)

| # | Given | Then |
|---|---|---|
| R-10 | `9200-GETCARDXREF-BYACCT` `READ DATASET('CXACAIX ') RIDFLD(account)` NORMAL | `CDEMO-CUST-ID ← XREF-CUST-ID`; `CDEMO-CARD-NUM ← XREF-CARD-NUM`. |
| R-11 | `NOTFND` | `INPUT-ERROR`; `FLG-ACCTFILTER-NOT-OK`; message `Account:<11-digit id> not found in Cross ref file.  Resp: Reas:` (the RESP values are **not** appended — literal text only). Stop. |
| R-12 | Other RESP | `INPUT-ERROR`; message `File Error: READ on CXACAIX  Resp: <resp> Reas: <reas>` form (`WS-FILE-ERROR-MESSAGE`). Stop. |
| R-13 | `9300-GETACCTDATA-BYACCT` `READ DATASET('ACCTDAT ')` NORMAL | `FOUND-ACCT-IN-MASTER`. `NOTFND` → `Account:<id> not found in Acct Master file.Resp: Reas:`; other → file error message. Stop on failure. |
| R-14 | `9400-GETCUSTDATA-BYCUST` `READ DATASET('CUSTDAT ') RIDFLD(CDEMO-CUST-ID X(9))` NORMAL | `FOUND-CUST-IN-MASTER`. `NOTFND` → `FLG-CUSTFILTER-NOT-OK`, message `CustId:<9-digit id> not found in customer master.Resp:  REAS:`; other → file error message. |

## 1200-SETUP-SCREEN-VARS (display)

| # | Given | Then |
|---|---|---|
| R-15 | Account or customer found | Account fields: `ACSTTUS←ACCT-ACTIVE-STATUS`, `ACURBAL←ACCT-CURR-BAL`, `ACRDLIM←ACCT-CREDIT-LIMIT`, `ACSHLIM←ACCT-CASH-CREDIT-LIMIT`, `ACRCYCR←ACCT-CURR-CYC-CREDIT`, `ACRCYDB←ACCT-CURR-CYC-DEBIT`, `ADTOPEN←ACCT-OPEN-DATE`, `AEXPDT←ACCT-EXPIRAION-DATE`, `AREISDT←ACCT-REISSUE-DATE`, `AADDGRP←ACCT-GROUP-ID`. Amounts are moved raw (S9(10)V99 → map PIC per `COACTVW` BMS, no explicit editing in the program). |
| R-16 | Customer found | `ACSTNUM←CUST-ID`; `ACSTSSN ← SSN(1:3)-SSN(4:2)-SSN(6:4)` (e.g. `123-45-6789`); `ACSTFCO←CUST-FICO-CREDIT-SCORE`; `ACSTDOB←CUST-DOB-YYYY-MM-DD`; first/middle/last name; address lines 1–3 (`ACSCITY←CUST-ADDR-LINE-3`), state, zip, country; phone 1/2; `ACSGOVT←CUST-GOVT-ISSUED-ID`; `ACSEFTC←CUST-EFT-ACCOUNT-ID`; `ACSPFLG←CUST-PRI-CARD-HOLDER-IND`. |
| R-17 | Info message | After a successful read, `WS-INFO-MSG` is blank (`9000-READ-ACCT` sets `WS-NO-INFO-MESSAGE`) so the prompt `Enter or update id of account to display` is shown again; `Displaying details of given Account` is defined but never set. `ERRMSGO ← WS-RETURN-MSG`. |
| R-18 | Attributes | `ACCTSID` always unprotected (`DFHBMFSE`), cursor on it; red when `FLG-ACCTFILTER-NOT-OK`; `*` red when blank on re-entry. Info message dark when blank, else neutral. |
| R-19 | Send | `CCARD-NEXT-MAP/MAPSET ← CACTVWA/COACTVW`; `CDEMO-PGM-REENTER`; `SEND MAP CURSOR ERASE FREEKB`. |

## ABEND-ROUTINE

| # | Given | Then |
|---|---|---|
| R-20 | Abend | Default `UNEXPECTED ABEND OCCURRED.`; culprit `COACTVWC`; send `ABEND-DATA`; `HANDLE ABEND CANCEL`; `ABEND ABCODE('9999')`. |
