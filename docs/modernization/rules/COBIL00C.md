# COBIL00C — Bill payment (transaction CB00, map COBIL0A / mapset COBIL00)

Source: `app/cbl/COBIL00C.cbl`. Data access:
`ACCTDAT` KSDS `READ ... UPDATE` + `REWRITE` by `ACCT-ID X(11)` (`CVACT01Y`);
`CXACAIX` (card-xref alternate index by account) `READ` by `XREF-ACCT-ID X(11)` (`CVACT03Y`);
`TRANSACT` KSDS `STARTBR`/`READPREV`/`ENDBR` from HIGH-VALUES and `WRITE` by `TRAN-ID X(16)` (`CVTRA05Y`).
Screen fields: `ACTIDIN` (11), `CURBAL` (display, `PIC +9999999999.99`), `CONFIRM` (1).
Commarea extension `CDEMO-CB00-INFO` (same shape as CT00; `CDEMO-CB00-TRN-SELECTED X(16)` is reused as a pre-filled account id).

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry | Set re-enter; clear map; cursor `ACTIDIN`; if `CDEMO-CB00-TRN-SELECTED` non-blank it is copied to `ACTIDINI` and `PROCESS-ENTER-KEY` runs; send. |
| R-3 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY`. |
| R-4 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM` = `CDEMO-FROM-PROGRAM` if non-blank else `COMEN01C`; `RETURN-TO-PREV-SCREEN`. |
| R-5 | Re-entry + `DFHPF4` | `CLEAR-CURRENT-SCREEN` (`ACTIDIN`, `CURBAL`, `CONFIRM`, message ← spaces; send). |
| R-6 | Other AID | `Invalid key pressed. Please see below...`; re-send. Non-XCTL paths `RETURN TRANSID('CB00') COMMAREA(...)`. |

## PROCESS-ENTER-KEY (SEND-BILLPAY-SCREEN does **not** RETURN, so later steps are guarded by `ERR-FLG`)

| # | Given | Then |
|---|---|---|
| R-7 | `ACTIDINI` blank | `WS-ERR-FLG='Y'`; `Acct ID can NOT be empty...`; cursor `ACTIDIN`; send. |
| R-8 | Present | `ACCT-ID` and `XREF-ACCT-ID ← ACTIDINI` as typed (no numeric check). `CONFIRMI`: `Y`/`y` → `CONF-PAY-YES`, `READ-ACCTDAT-FILE`; `N`/`n` → `CLEAR-CURRENT-SCREEN` and `ERR-FLG='Y'` (ends processing, no message); blank → `READ-ACCTDAT-FILE`; other → `ERR-FLG='Y'`, `Invalid value. Valid values are (Y/N)...`, cursor `CONFIRM`, send. |
| R-9 | Account read OK (R-16) | `CURBALI ← ACCT-CURR-BAL` edited `+9999999999.99`. |
| R-10 | No error and `ACCT-CURR-BAL <= 0` | `ERR-FLG='Y'`; `You have nothing to pay...`; cursor `ACTIDIN`; send. |
| R-11 | No error, `CONF-PAY-NO` (blank confirm) | Message `Confirm to make a bill payment...`; cursor `CONFIRM`; send (balance shown, awaiting Y). |
| R-12 | No error, `CONF-PAY-YES` | `READ-CXACAIX-FILE` (card for the account, R-18); `TRAN-ID ← HIGH-VALUES`, `STARTBR`, `READPREV` (last transaction; ENDFILE → `TRAN-ID ← 0`), `ENDBR`; `WS-TRAN-ID-NUM ← TRAN-ID + 1`. New `TRAN-RECORD`: `TRAN-ID ← WS-TRAN-ID-NUM` (16 digits zero-padded), `TRAN-TYPE-CD='02'`, `TRAN-CAT-CD=2` (→`0002`), `TRAN-SOURCE='POS TERM'`, `TRAN-DESC='BILL PAYMENT - ONLINE'`, `TRAN-AMT ← ACCT-CURR-BAL` (full balance, positive), `TRAN-CARD-NUM ← XREF-CARD-NUM`, `TRAN-MERCHANT-ID=999999999`, `TRAN-MERCHANT-NAME='BILL PAYMENT'`, `TRAN-MERCHANT-CITY='N/A'`, `TRAN-MERCHANT-ZIP='N/A'`, `TRAN-ORIG-TS = TRAN-PROC-TS ← current timestamp` (R-13). `WRITE-TRANSACT-FILE` (R-20); then `ACCT-CURR-BAL ← ACCT-CURR-BAL - TRAN-AMT` (= 0) and `UPDATE-ACCTDAT-FILE` (R-17). |
| R-13 | `GET-CURRENT-TIMESTAMP` | `ASKTIME`/`FORMATTIME YYYYMMDD DATESEP('-') TIME TIMESEP(':')` → `WS-TIMESTAMP = 'YYYY-MM-DD HH:MM:SS.000000'` form: date in (1:10), time in (12:8), microseconds zero. |

Arithmetic: payment amount = current balance; new balance = 0.00. No partial payments; no credit-limit or cycle fields touched
(`ACCT-CURR-CYC-CREDIT/DEBIT` unchanged — contrast batch CBTRN02C which updates cycle debit).

## File paragraphs

| # | Given | Then |
|---|---|---|
| R-16 | `READ-ACCTDAT-FILE` (`READ UPDATE`) | `NOTFND` → `Account ID NOT found...`; other → `Unable to lookup Account...`; both set `ERR-FLG`, cursor `ACTIDIN`, send. |
| R-17 | `UPDATE-ACCTDAT-FILE` (`REWRITE`) | `NOTFND` → `Account ID NOT found...`; other → `Unable to Update Account...`. Note the transaction has already been written (no syncpoint rollback is coded). |
| R-18 | `READ-CXACAIX-FILE` | `NOTFND` → `Account ID NOT found...`; other → `Unable to lookup XREF AIX file...`. Processing continues even on error (no guard) — the write then uses whatever `XREF-CARD-NUM` holds. |
| R-19 | `STARTBR-TRANSACT-FILE` | `NOTFND` → `Transaction ID NOT found...`; other → `Unable to lookup Transaction...`. `READPREV` `ENDFILE` → `TRAN-ID ← ZEROS` (first ever id becomes `0000000000000001`). |
| R-20 | `WRITE-TRANSACT-FILE` | `NORMAL` → fields cleared, `ERRMSGC=DFHGREEN`, message `Payment successful.  Your Transaction ID is <id>.` (two spaces before *Your*); `DUPKEY`/`DUPREC` → `Tran ID already exist...`; other → `Unable to Add Bill pay Transaction...`. |

## RETURN-TO-PREV-SCREEN / SEND-BILLPAY-SCREEN

| # | Given | Then |
|---|---|---|
| R-21 | Return | Blank target → `COSGN00C`; from-fields `CB00`/`COBIL00C`, context 0; `XCTL ... COMMAREA`. |
| R-22 | Send | Standard header (`CB00`, `COBIL00C`); `SEND MAP('COBIL0A') MAPSET('COBIL00') ERASE CURSOR`. Multiple sends per task are possible; the last one wins. |
