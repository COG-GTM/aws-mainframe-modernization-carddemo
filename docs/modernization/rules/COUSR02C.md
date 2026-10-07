# COUSR02C — Update user (transaction CU02, map COUSR2A / mapset COUSR02)

Source: `app/cbl/COUSR02C.cbl`. Data access: `USRSEC` KSDS, `READ ... UPDATE` then `REWRITE` (key `SEC-USR-ID`).
Commarea extension `CDEMO-CU02-INFO.CDEMO-CU02-USR-SELECTED X(8)` (user id pre-selected in COUSR00C with `U`).
Fields: `USRIDIN` (8, key), `FNAME`, `LNAME`, `PASSWD`, `USRTYPE`.

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry, `CDEMO-CU02-USR-SELECTED` blank | Clear map, cursor `USRIDIN`, send screen (empty). |
| R-3 | First entry, `CDEMO-CU02-USR-SELECTED` non-blank | It is moved to `USRIDINI` and `PROCESS-ENTER-KEY` runs immediately (user record pre-loaded, R-9/R-10), then the screen is sent. |
| R-4 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY` (lookup). |
| R-5 | Re-entry + `DFHPF3` | `UPDATE-USER-INFO` is performed **first** (an implicit save attempt, R-11 ff.), then `CDEMO-TO-PROGRAM` = `CDEMO-FROM-PROGRAM` if non-blank else `COADM01C`, `RETURN-TO-PREV-SCREEN`. |
| R-6 | Re-entry + `DFHPF4` | `CLEAR-CURRENT-SCREEN` (all fields + message blank, cursor `USRIDIN`). |
| R-7 | Re-entry + `DFHPF5` | `UPDATE-USER-INFO` (save). |
| R-8 | Re-entry + `DFHPF12` | `CDEMO-TO-PROGRAM='COADM01C'`; `RETURN-TO-PREV-SCREEN`. |
| R-8a | Other AID | `WS-ERR-FLG='Y'`; `Invalid key pressed. Please see below...`; re-send. Non-XCTL paths end with `RETURN TRANSID('CU02') COMMAREA(...)`. |

## PROCESS-ENTER-KEY — lookup

| # | Given | Then |
|---|---|---|
| R-9 | `USRIDINI` spaces/low-values | `WS-ERR-FLG='Y'`; `User ID can NOT be empty...`; cursor `USRIDIN`. |
| R-10 | Present | Detail fields blanked, `SEC-USR-ID ← USRIDINI`, `READ-USER-SEC-FILE` (R-17..R-19). On success `FNAME/LNAME/PASSWD/USRTYPE` ← record values (password is displayed in clear) and screen sent. |

## UPDATE-USER-INFO — save (PF5, and implicitly PF3)

| # | Given | Then |
|---|---|---|
| R-11 | `USRIDINI` blank | `User ID can NOT be empty...`, cursor `USRIDIN`. |
| R-12 | `FNAMEI` blank | `First Name can NOT be empty...`, cursor `FNAME`. |
| R-13 | `LNAMEI` blank | `Last Name can NOT be empty...`, cursor `LNAME`. |
| R-14 | `PASSWDI` blank | `Password can NOT be empty...`, cursor `PASSWD`. |
| R-15 | `USRTYPEI` blank | `User Type can NOT be empty...`, cursor `USRTYPE`. |
| R-16 | All present | `READ-USER-SEC-FILE` with UPDATE (this also sends the `Press PF5 ...` screen, see R-17), then each of FNAME, LNAME, PWD, TYPE is compared with the record; differing values are copied in and `USR-MODIFIED-YES` set. If modified → `UPDATE-USER-SEC-FILE` (R-20..R-22); else `ERRMSGC=DFHRED`, message `Please modify to update ...`, re-send. |

## READ-USER-SEC-FILE (`READ ... UPDATE`, full key, 8 bytes)

| # | Given | Then |
|---|---|---|
| R-17 | `NORMAL` | Message `Press PF5 key to save your updates ...`, `ERRMSGC=DFHNEUTR`, screen sent. (Also fires during R-16 before the rewrite.) |
| R-18 | `NOTFND` | `WS-ERR-FLG='Y'`; `User ID NOT found...`; cursor `USRIDIN`. |
| R-19 | Other | `DISPLAY 'RESP:'...` to CICS log; `WS-ERR-FLG='Y'`; `Unable to lookup User...`; cursor `FNAME`. |

## UPDATE-USER-SEC-FILE (`REWRITE FROM(SEC-USER-DATA)`)

| # | Given | Then |
|---|---|---|
| R-20 | `NORMAL` | `ERRMSGC=DFHGREEN`; `User <id> has been updated ...`; fields **kept** (not cleared). |
| R-21 | `NOTFND` | `User ID NOT found...`, cursor `USRIDIN`. |
| R-22 | Other | `Unable to Update User...`, cursor `FNAME`. |

## RETURN-TO-PREV-SCREEN / SEND

| # | Given | Then |
|---|---|---|
| R-23 | Return | Blank target → `COSGN00C`; from-fields = `CU02`/`COUSR02C`, context 0; `XCTL ... COMMAREA`. |
| R-24 | Send | Standard header (`CU02`, `COUSR02C`); `SEND MAP('COUSR2A') MAPSET('COUSR02') ERASE CURSOR`. |
