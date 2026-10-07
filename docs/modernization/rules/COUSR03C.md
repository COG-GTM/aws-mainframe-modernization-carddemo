# COUSR03C — Delete user (transaction CU03, map COUSR3A / mapset COUSR03)

Source: `app/cbl/COUSR03C.cbl`. Data access: `USRSEC` KSDS, `READ ... UPDATE` then `DELETE` (key `SEC-USR-ID`).
Commarea extension `CDEMO-CU03-USR-SELECTED X(8)` (pre-selected in COUSR00C with `D`).
Fields: `USRIDIN` (8), display-only `FNAME`, `LNAME`, `USRTYPE` (no password shown).

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry, no pre-selected user | Clear map, cursor `USRIDIN`, send. |
| R-3 | First entry, `CDEMO-CU03-USR-SELECTED` non-blank | Copied to `USRIDINI`; `PROCESS-ENTER-KEY` (lookup) runs before the send. |
| R-4 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY`. |
| R-5 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM` = `CDEMO-FROM-PROGRAM` if non-blank else `COADM01C`; `RETURN-TO-PREV-SCREEN`. (No implicit delete — contrast COUSR02C R-5.) |
| R-6 | Re-entry + `DFHPF4` | `CLEAR-CURRENT-SCREEN`. |
| R-7 | Re-entry + `DFHPF5` | `DELETE-USER-INFO`. |
| R-8 | Re-entry + `DFHPF12` | `CDEMO-TO-PROGRAM='COADM01C'`; return. |
| R-9 | Other AID | `Invalid key pressed. Please see below...`; re-send. All non-XCTL paths `RETURN TRANSID('CU03')`. |

## PROCESS-ENTER-KEY — lookup

| # | Given | Then |
|---|---|---|
| R-10 | `USRIDINI` blank | `WS-ERR-FLG='Y'`; `User ID can NOT be empty...`; cursor `USRIDIN`. |
| R-11 | Present | Display fields blanked; `READ-USER-SEC-FILE` (R-14..R-16); on success `FNAME/LNAME/USRTYPE` filled and screen sent. |

## DELETE-USER-INFO (PF5)

| # | Given | Then |
|---|---|---|
| R-12 | `USRIDINI` blank | `User ID can NOT be empty...`. |
| R-13 | Present | `READ-USER-SEC-FILE` (UPDATE lock; also sends the `Press PF5 ...` screen) then `DELETE-USER-SEC-FILE` (R-17..R-19), even if the read failed (the DELETE then returns NOTFND/INVREQ and R-18/R-19 apply). |

## READ-USER-SEC-FILE

| # | Given | Then |
|---|---|---|
| R-14 | `NORMAL` | Message `Press PF5 key to delete this user ...`, `ERRMSGC=DFHNEUTR`, screen sent. |
| R-15 | `NOTFND` | `WS-ERR-FLG='Y'`; `User ID NOT found...`; cursor `USRIDIN`. |
| R-16 | Other | `Unable to lookup User...`; cursor `FNAME`. |

## DELETE-USER-SEC-FILE (`EXEC CICS DELETE DATASET('USRSEC  ')` — deletes the record locked by the preceding READ UPDATE)

| # | Given | Then |
|---|---|---|
| R-17 | `NORMAL` | All fields cleared, cursor `USRIDIN`, `ERRMSGC=DFHGREEN`, `User <id> has been deleted ...`. |
| R-18 | `NOTFND` | `User ID NOT found...`, cursor `USRIDIN`. |
| R-19 | Other | `Unable to Update User...` (sic — message text says *Update*), cursor `FNAME`. |

## RETURN-TO-PREV-SCREEN / SEND

| # | Given | Then |
|---|---|---|
| R-20 | Return | Blank target → `COSGN00C`; from-fields `CU03`/`COUSR03C`, context 0; `XCTL ... COMMAREA`. |
| R-21 | Send | Standard header (`CU03`, `COUSR03C`); `SEND MAP('COUSR3A') MAPSET('COUSR03') ERASE CURSOR`. |

Note: no self-delete guard (an admin may delete the signed-on user) and no "last admin" guard.
