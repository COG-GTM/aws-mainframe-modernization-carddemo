# COUSR00C — User list (transaction CU00, map COUSR0A / mapset COUSR00)

Source: `app/cbl/COUSR00C.cbl`. Data access: `USRSEC` KSDS, browse (`STARTBR`/`READNEXT`/`READPREV`/`ENDBR`), key
`SEC-USR-ID X(8)`, record `CSUSR01Y`. Page size 10. Commarea extension `CDEMO-CU00-INFO`:
`USRID-FIRST X(8)`, `USRID-LAST X(8)`, `PAGE-NUM 9(8)`, `NEXT-PAGE-FLG X(1)`, `USR-SEL-FLG X(1)`, `USR-SELECTED X(8)`.
Screen: filter `USRIDIN` (8), 10 rows of `SELnnnn`(1) / `USRIDnn`(8) / `FNAMEnn`(20) / `LNAMEnn`(20) / `UTYPEnn`(1), `PAGENUM`.

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | Each invocation | `ERR-FLG`, `USER-SEC-EOF`, `NEXT-PAGE` reset to N/not-EOF/N; `SEND-ERASE-YES`; message blank; cursor `USRIDIN`. |
| R-2 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-3 | First entry (`CDEMO-PGM-CONTEXT=0`) | Set re-enter; clear map; `PROCESS-ENTER-KEY` (initial page 1 from the lowest key); screen sent. |
| R-4 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY`. |
| R-5 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM='COADM01C'`; `RETURN-TO-PREV-SCREEN`. |
| R-6 | Re-entry + `DFHPF7` | `PROCESS-PF7-KEY` (page back). |
| R-7 | Re-entry + `DFHPF8` | `PROCESS-PF8-KEY` (page forward). |
| R-8 | Other AID | `WS-ERR-FLG='Y'`; `Invalid key pressed. Please see below...`; re-send. All non-XCTL paths `RETURN TRANSID('CU00') COMMAREA(...)`. |

## PROCESS-ENTER-KEY — selection then (re)list

| # | Given | Then |
|---|---|---|
| R-9 | First row (top-down, rows 1..10) whose `SELnnnnI` is non-blank | `CDEMO-CU00-USR-SEL-FLG ← SELnnnnI`, `CDEMO-CU00-USR-SELECTED ← USRIDnnI` of that row; only the first marked row counts. No row marked → both blank. |
| R-10 | Sel flag and selected id both non-blank, flag `U`/`u` | `CDEMO-TO-PROGRAM='COUSR02C'`; from-fields `CU00`/`COUSR00C`, context 0; `XCTL PROGRAM('COUSR02C') COMMAREA(...)`. COUSR02C then pre-loads that user (COUSR02C R-3). |
| R-11 | Flag `D`/`d` | Same, `XCTL PROGRAM('COUSR03C')`. |
| R-12 | Any other flag character | Message `Invalid selection. Valid values are U and D`; cursor `USRIDIN`; processing **continues** into the re-list (R-13). |
| R-13 | No XCTL | Browse start key: `USRIDINI` blank → `LOW-VALUES`, else `USRIDINI` as typed (prefix positioning, no validation). `CDEMO-CU00-PAGE-NUM ← 0`; `PROCESS-PAGE-FORWARD`. If no error, `USRIDINO` cleared on the screen. |

## PROCESS-PF7-KEY (backward)

| # | Given | Then |
|---|---|---|
| R-14 | `CDEMO-CU00-PAGE-NUM > 1` | Browse key ← `CDEMO-CU00-USRID-FIRST` (first id on the current page; blank → LOW-VALUES); `NEXT-PAGE-YES`; `PROCESS-PAGE-BACKWARD`. |
| R-15 | Page ≤ 1 | Message `You are already at the top of the page...`; screen re-sent **without** ERASE. |

## PROCESS-PF8-KEY (forward)

| # | Given | Then |
|---|---|---|
| R-16 | `NEXT-PAGE-YES` in commarea | Browse key ← `CDEMO-CU00-USRID-LAST` (blank → HIGH-VALUES); `PROCESS-PAGE-FORWARD`. |
| R-17 | `NEXT-PAGE-NO` | Message `You are already at the bottom of the page...`; re-sent without ERASE. |

## PROCESS-PAGE-FORWARD

| # | Given | Then |
|---|---|---|
| R-18 | Start | `STARTBR` at key (R-24). If `EIBAID` is not ENTER/PF7/PF3 (i.e. PF8) one `READNEXT` is consumed first to skip the last row of the previous page. |
| R-19 | Not EOF | Rows 1–10 cleared, then up to 10 `READNEXT`s populate rows i=1..10: `USRIDnn←SEC-USR-ID`, `FNAMEnn←SEC-USR-FNAME`, `LNAMEnn←SEC-USR-LNAME`, `UTYPEnn←SEC-USR-TYPE`; row 1 id → `CDEMO-CU00-USRID-FIRST`, row 10 id → `CDEMO-CU00-USRID-LAST`. |
| R-20 | 10 rows filled and file not at EOF | `PAGE-NUM += 1`; one look-ahead `READNEXT`: success → `NEXT-PAGE-YES`, ENDFILE → `NEXT-PAGE-NO`. |
| R-21 | Fewer than 10 rows (EOF hit) | `NEXT-PAGE-NO`; `PAGE-NUM += 1` only if at least one row was filled (`WS-IDX > 1`). |
| R-22 | Always | `ENDBR`; `PAGENUMI ← PAGE-NUM`; `USRIDINO` cleared; screen sent. |

## PROCESS-PAGE-BACKWARD

| # | Given | Then |
|---|---|---|
| R-23 | Start | `STARTBR` at `USRID-FIRST`; if `EIBAID` not ENTER/PF8 (i.e. PF7) one `READPREV` consumed; rows cleared; rows filled from 10 down to 1 by `READPREV` (so the page ends at the record before the old first row). Then one more `READPREV`; if `NEXT-PAGE-YES` and that read succeeded and `PAGE-NUM > 1` → `PAGE-NUM -= 1`, else `PAGE-NUM ← 1`. `ENDBR`; `PAGENUMI ← PAGE-NUM`; send. |

## Browse primitives

| # | Given | Then |
|---|---|---|
| R-24 | `STARTBR` `NOTFND` (key beyond last record) | `USER-SEC-EOF`; message `You are at the top of the page...`; send. Other error → `WS-ERR-FLG='Y'`, `Unable to lookup User...`. |
| R-25 | `READNEXT` `ENDFILE` | `USER-SEC-EOF`; message `You have reached the bottom of the page...`; send. Other error → `Unable to lookup User...`. |
| R-26 | `READPREV` `ENDFILE` | `USER-SEC-EOF`; message `You have reached the top of the page...`; send. Other error → `Unable to lookup User...`. |

Note: because the EOF messages are sent inside the primitives and the page is sent again afterwards, the last SEND wins; the
page with the partially-filled rows is what the user sees, with the EOF message still in `WS-MESSAGE`.

## RETURN-TO-PREV-SCREEN / SEND-USRLST-SCREEN

| # | Given | Then |
|---|---|---|
| R-27 | Return | Blank target → `COSGN00C`; from-fields `CU00`/`COUSR00C`; context 0; `XCTL ... COMMAREA`. |
| R-28 | Send | Standard header (`CU00`, `COUSR00C`); `SEND MAP('COUSR0A') MAPSET('COUSR00') CURSOR`, with `ERASE` unless `SEND-ERASE-NO` (R-15/R-17). |
