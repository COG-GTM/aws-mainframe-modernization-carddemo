# COADM01C — Admin menu (transaction CA00, map COADM1A / mapset COADM01)

Source: `app/cbl/COADM01C.cbl`; option table `COADM02Y`. No file access. Reached from `COSGN00C` only when
`CDEMO-USER-TYPE='A'` (COSGN00C R-10). Structure is identical to `COMEN01C`; differences are called out.

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-FROM-PROGRAM='COSGN00C'`; `XCTL PROGRAM('COSGN00C')` (no commarea). |
| R-2 | First entry (`CDEMO-PGM-CONTEXT=0`) | Set re-enter, clear map, `SEND-MENU-SCREEN`. |
| R-3 | Re-entry + `DFHENTER` | `RECEIVE MAP('COADM1A')`, `PROCESS-ENTER-KEY`. |
| R-4 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM='COSGN00C'`, `XCTL PROGRAM('COSGN00C')` (no commarea). |
| R-5 | Re-entry + other AID | `WS-ERR-FLG='Y'`, `Invalid key pressed. Please see below...`, re-send. |
| R-6 | Non-XCTL paths | `RETURN TRANSID('CA00') COMMAREA(CARDDEMO-COMMAREA)`. |

## PROCESS-ENTER-KEY

| # | Given | Then |
|---|---|---|
| R-7 | `OPTIONI` | Same normalisation as COMEN01C R-7 (right-trim, spaces→`0`, `PIC 9(02)`), echoed to `OPTIONO`. |
| R-8 | Not numeric, `> CDEMO-ADMIN-OPT-COUNT` (6) or `= 0` | `WS-ERR-FLG='Y'`; `Please enter a valid option number...`; re-send. |
| R-9 | Valid option whose program does **not** start with `DUMMY` | `CDEMO-FROM-TRANID='CA00'`, `CDEMO-FROM-PROGRAM='COADM01C'`, `CDEMO-PGM-CONTEXT=0`; `XCTL PROGRAM(CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)) COMMAREA(...)`. |
| R-10 | Valid option whose program starts with `DUMMY`, or the XCTL in R-9 returns (it does not under CICS unless PGMIDERR) | `ERRMSGC=DFHGREEN`; message `This option is not installed ...`; re-send. Note there is **no** user-type check in the admin menu (contrast COMEN01C R-9). |
| R-11 | `PGMIDERR-ERR-PARA`, entered through `HANDLE CONDITION PGMIDERR(PGMIDERR-ERR-PARA)` at the top of `MAIN-PARA` (corrected in UNT51-17: the handler is active, the paragraph is not dead code) | An XCTL in R-9 to a program that is not installed (`COTRTLIC`, `COTRTUPC` in the core estate) sets `ERRMSGC=DFHGREEN`, message `This option is not installed ...`, re-sends the menu and RETURNs with TRANSID `CA00`; no abend. |

Option table (`COADM02Y`, `CDEMO-ADMIN-OPT-COUNT = 6`):

| Opt | Name | Program |
|---|---|---|
| 01 | User List (Security) | COUSR00C |
| 02 | User Add (Security) | COUSR01C |
| 03 | User Update (Security) | COUSR02C |
| 04 | User Delete (Security) | COUSR03C |
| 05 | Transaction Type List/Update (Db2) | COTRTLIC (Db2 variant, not in this estate → PGMIDERR abend at XCTL) |
| 06 | Transaction Type Maintenance (Db2) | COTRTUPC (idem) |

## SEND-MENU-SCREEN / BUILD-MENU-OPTIONS

| # | Given | Then |
|---|---|---|
| R-12 | Every send | Standard header (`TRNNAME='CA00'`, `PGMNAME='COADM01C'`), `ERRMSG ← WS-MESSAGE`, `SEND MAP('COADM1A') MAPSET('COADM01') ERASE`. |
| R-13 | Option lines | `OPTN00iO = '<NN>. <name>'` for i = 1..6; fields 7–12 blank (the map has 12 option fields). |
