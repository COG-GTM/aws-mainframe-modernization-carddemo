# COUSR01C — Add user (transaction CU01, map COUSR1A / mapset COUSR01)

Source: `app/cbl/COUSR01C.cbl`. Data access: `USRSEC` KSDS, **WRITE** (key `SEC-USR-ID` X(8)), record `CSUSR01Y`.
Input fields: `FNAME` (20), `LNAME` (20), `USERID` (8), `PASSWD` (8), `USRTYPE` (1).

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN` (R-14). |
| R-2 | First entry (`CDEMO-PGM-CONTEXT=0`) | Set re-enter; clear map; cursor on `FNAME`; `SEND-USRADD-SCREEN`. |
| R-3 | Re-entry + `DFHENTER` | `RECEIVE MAP('COUSR1A')`; `PROCESS-ENTER-KEY`. |
| R-4 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM='COADM01C'`; `RETURN-TO-PREV-SCREEN`. |
| R-5 | Re-entry + `DFHPF4` | `CLEAR-CURRENT-SCREEN`: all five input fields and `WS-MESSAGE` ← spaces, cursor on `FNAME`, screen re-sent. |
| R-6 | Other AID | `WS-ERR-FLG='Y'`; cursor `FNAME`; `Invalid key pressed. Please see below...`; re-send. |
| R-7 | Non-XCTL paths | `RETURN TRANSID('CU01') COMMAREA(...)`. |

## PROCESS-ENTER-KEY — validation (first failing rule wins, order as listed)

| # | Given | Then |
|---|---|---|
| R-8 | `FNAMEI` spaces/low-values | `First Name can NOT be empty...`, cursor `FNAME`. |
| R-9 | `LNAMEI` spaces/low-values | `Last Name can NOT be empty...`, cursor `LNAME`. |
| R-10 | `USERIDI` spaces/low-values | `User ID can NOT be empty...`, cursor `USERID`. |
| R-11 | `PASSWDI` spaces/low-values | `Password can NOT be empty...`, cursor `PASSWD`. |
| R-12 | `USRTYPEI` spaces/low-values | `User Type can NOT be empty...`, cursor `USRTYPE`. |
| R-13 | All present | Cursor `FNAME`; `SEC-USER-DATA` built from the five fields **as typed** (no upper-casing, no trim, no type-value check: any single character is accepted as user type); `WRITE-USER-SEC-FILE`. |

Each failure sets `WS-ERR-FLG='Y'` and re-sends the screen with the message.

## WRITE-USER-SEC-FILE

`EXEC CICS WRITE DATASET('USRSEC  ') FROM(SEC-USER-DATA) RIDFLD(SEC-USR-ID) KEYLENGTH(8)`

| # | Given | Then |
|---|---|---|
| R-15 | `RESP = NORMAL` | All input fields cleared (`INITIALIZE-ALL-FIELDS`), cursor `FNAME`, `ERRMSGC=DFHGREEN`, message `User <id> has been added ...` where `<id>` is `SEC-USR-ID` up to its first space (e.g. `User JDOE01 has been added ...`). |
| R-16 | `RESP = DUPKEY` or `DUPREC` | `WS-ERR-FLG='Y'`; `User ID already exist...`; cursor `USERID`; fields retained. |
| R-17 | Any other RESP | `WS-ERR-FLG='Y'`; `Unable to Add User...`; cursor `FNAME`. |

## RETURN-TO-PREV-SCREEN

| # | Given | Then |
|---|---|---|
| R-14 | Called | If `CDEMO-TO-PROGRAM` blank → `COSGN00C`. Set `CDEMO-FROM-TRANID='CU01'`, `CDEMO-FROM-PROGRAM='COUSR01C'`, `CDEMO-PGM-CONTEXT=0`; `XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)`. |

## SEND-USRADD-SCREEN

| # | Given | Then |
|---|---|---|
| R-18 | Every send | Standard header (`TRNNAME='CU01'`, `PGMNAME='COUSR01C'`); `SEND MAP('COUSR1A') MAPSET('COUSR01') ERASE CURSOR`. |
