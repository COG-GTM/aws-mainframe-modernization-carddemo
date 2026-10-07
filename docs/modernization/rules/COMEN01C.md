# COMEN01C — Main menu (transaction CM00, map COMEN1A / mapset COMEN01)

Source: `app/cbl/COMEN01C.cbl`; option table copybook `COMEN02Y`. No file access. Commarea `COCOM01Y`.
Messages verbatim; `CCDA-MSG-INVALID-KEY` is `Invalid key pressed. Please see below...` (`CSMSG01Y`).

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-FROM-PROGRAM='COSGN00C'`; `RETURN-TO-SIGNON-SCREEN` → `XCTL PROGRAM('COSGN00C')` (no commarea). |
| R-2 | Commarea present and `CDEMO-PGM-CONTEXT = 0` (`NOT CDEMO-PGM-REENTER`) | Set `CDEMO-PGM-REENTER` (=1), clear map, `SEND-MENU-SCREEN`. |
| R-3 | Re-entry (`CDEMO-PGM-CONTEXT = 1`), `EIBAID = DFHENTER` | `RECEIVE MAP('COMEN1A')` then `PROCESS-ENTER-KEY`. |
| R-4 | Re-entry, `DFHPF3` | `CDEMO-TO-PROGRAM='COSGN00C'`; `XCTL PROGRAM('COSGN00C')` without commarea → sign-on screen in fresh state. |
| R-5 | Re-entry, any other AID | `WS-ERR-FLG='Y'`; message `Invalid key pressed. Please see below...`; menu re-sent. |
| R-6 | Every non-XCTL path | `RETURN TRANSID('CM00') COMMAREA(CARDDEMO-COMMAREA)`. |

## PROCESS-ENTER-KEY — option parsing

| # | Given | Then |
|---|---|---|
| R-7 | `OPTIONI` (2 chars) entered | Trailing spaces are trimmed (scan from the right to the last non-space), then every remaining space is replaced by `'0'` and the result moved to `WS-OPTION PIC 9(02)`. So `"1 "`→`01`, `" 1"`→`01`, `"7"`→`07`. The normalised value is echoed back in `OPTIONO`. |
| R-8 | `WS-OPTION` not numeric, or `> CDEMO-MENU-OPT-COUNT` (11), or `= 0` | `WS-ERR-FLG='Y'`; message `Please enter a valid option number...`; menu re-sent. |
| R-9 | User type is `'U'` (`CDEMO-USRTYP-USER`) and the option's `CDEMO-MENU-OPT-USRTYPE = 'A'` | `ERR-FLG-ON`; message `No access - Admin Only option... `; menu re-sent. (All 11 options in `COMEN02Y` are `'U'`, so this rule is currently unreachable but must be preserved.) |
| R-10 | Valid option whose program is `COPAUS0C` (option 11) | `EXEC CICS INQUIRE PROGRAM(...) NOHANDLE`; if `EIBRESP = NORMAL` → set from-fields (`CDEMO-FROM-TRANID='CM00'`, `CDEMO-FROM-PROGRAM='COMEN01C'`, `CDEMO-PGM-CONTEXT=0`) and `XCTL` to it. Otherwise `ERRMSGC=DFHRED` and message `This option <option name, delimited by two spaces> is not installed...`, e.g. `This option Pending Authorization View is not installed...`. |
| R-11 | Valid option whose program name starts with `DUMMY` | `ERRMSGC=DFHGREEN`; message `This option <first word of option name>is coming soon ...` (note: `DELIMITED BY SPACE` keeps only the first word and there is no space before `is`). No DUMMY entries exist in `COMEN02Y` today. |
| R-12 | Any other valid option (1–10) | From-fields set as in R-10; `XCTL PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION)) COMMAREA(CARDDEMO-COMMAREA)`. |

Option table (`COMEN02Y`, `CDEMO-MENU-OPT-COUNT = 11`):

| Opt | Name | Program | Type |
|---|---|---|---|
| 01 | Account View | COACTVWC | U |
| 02 | Account Update | COACTUPC | U |
| 03 | Credit Card List | COCRDLIC | U |
| 04 | Credit Card View | COCRDSLC | U |
| 05 | Credit Card Update | COCRDUPC | U |
| 06 | Transaction List | COTRN00C | U |
| 07 | Transaction View | COTRN01C | U |
| 08 | Transaction Add | COTRN02C | U |
| 09 | Transaction Reports | CORPT00C | U |
| 10 | Bill Payment | COBIL00C | U |
| 11 | Pending Authorization View | COPAUS0C | U (not installed in this estate → R-10 message) |

## SEND-MENU-SCREEN / BUILD-MENU-OPTIONS

| # | Given | Then |
|---|---|---|
| R-13 | Every send | Standard header (titles from `COTTL01Y`, `TRNNAME='CM00'`, `PGMNAME='COMEN01C'`, date `MM/DD/YY`, time `HH:MM:SS`); `ERRMSG ← WS-MESSAGE`; `SEND MAP('COMEN1A') MAPSET('COMEN01') ERASE`. |
| R-14 | Option lines | For i = 1..11, `OPTN00iO` = `<NN>. <name>` where NN is the 2-digit `CDEMO-MENU-OPT-NUM` (e.g. `01. Account View`). Lines 12 are left blank (the map has 12 option fields). |
