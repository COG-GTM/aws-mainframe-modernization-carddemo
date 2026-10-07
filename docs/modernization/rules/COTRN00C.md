# COTRN00C — Transaction list (transaction CT00, map COTRN0A / mapset COTRN00)

Source: `app/cbl/COTRN00C.cbl`. Data access: `TRANSACT` KSDS browse (`STARTBR`/`READNEXT`/`READPREV`/`ENDBR`), key
`TRAN-ID X(16)`, record `CVTRA05Y`. Page size 10. Commarea extension `CDEMO-CT00-INFO`: `TRNID-FIRST X(16)`,
`TRNID-LAST X(16)`, `PAGE-NUM 9(8)`, `NEXT-PAGE-FLG`, `TRN-SEL-FLG X(1)`, `TRN-SELECTED X(16)`.
Screen: filter `TRNIDIN` (16), 10 rows `SELnnnn`(1)/`TRNIDnn`(16)/`TDATEnn`(8)/`TDESCnn`(26)/`TAMTnnn`(12), `PAGENUM`.
The paging algorithm is the same as COUSR00C (R-18..R-26 there); only the differences are listed.

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry | Set re-enter; clear map; `PROCESS-ENTER-KEY`; send. |
| R-3 | Re-entry + `DFHENTER` / `DFHPF7` / `DFHPF8` | `PROCESS-ENTER-KEY` / `PROCESS-PF7-KEY` / `PROCESS-PF8-KEY`. |
| R-4 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM='COMEN01C'`; `RETURN-TO-PREV-SCREEN`. |
| R-5 | Other AID | `Invalid key pressed. Please see below...`; re-send. Non-XCTL paths `RETURN TRANSID('CT00') COMMAREA(...)`. |

## PROCESS-ENTER-KEY

| # | Given | Then |
|---|---|---|
| R-6 | First row (1..10) with non-blank `SELnnnnI` | `CDEMO-CT00-TRN-SEL-FLG ← SELnnnnI`, `CDEMO-CT00-TRN-SELECTED ← TRNIDnnI`. None → both blank. |
| R-7 | Flag `S`/`s` and selected id non-blank | `CDEMO-TO-PROGRAM='COTRN01C'`; from-fields `CT00`/`COTRN00C`, context 0; `XCTL PROGRAM('COTRN01C') COMMAREA(...)` (COTRN01C R-3 shows the record). |
| R-8 | Any other flag | Message `Invalid selection. Valid value is S`; cursor `TRNIDIN`; continue to re-list. |
| R-9 | `TRNIDINI` blank | Browse key ← LOW-VALUES. |
| R-10 | `TRNIDINI` non-blank and `NUMERIC` | Browse key ← `TRNIDINI` (16 digits, exact-or-greater positioning). |
| R-11 | `TRNIDINI` non-blank and not numeric | `WS-ERR-FLG='Y'`; `Tran ID must be Numeric ...`; cursor `TRNIDIN`; screen sent (processing still falls through to `PROCESS-PAGE-FORWARD`, whose STARTBR with the stale key is suppressed only if an error occurs; the error-flag check at the end skips clearing `TRNIDINO`). |
| R-12 | Then | `PAGE-NUM ← 0`; `PROCESS-PAGE-FORWARD`; if no error, `TRNIDINO` cleared. |

## PF7 / PF8

| # | Given | Then |
|---|---|---|
| R-13 | PF7, `PAGE-NUM > 1` | Key ← `TRNID-FIRST` (blank → LOW-VALUES); `NEXT-PAGE-YES`; `PROCESS-PAGE-BACKWARD`. |
| R-14 | PF7, page ≤ 1 | `You are already at the top of the page...`; send without ERASE. |
| R-15 | PF8, `NEXT-PAGE-YES` | Key ← `TRNID-LAST` (blank → HIGH-VALUES); `PROCESS-PAGE-FORWARD`. |
| R-16 | PF8, `NEXT-PAGE-NO` | `You are already at the bottom of the page...`; send without ERASE. |

## POPULATE-TRAN-DATA (per row i)

| # | Given | Then |
|---|---|---|
| R-17 | Record read | `TRNIDnn ← TRAN-ID`; `TDESCnn ← TRAN-DESC` (truncated to 26); `TAMTnnn ← TRAN-AMT` edited as `PIC +99999999.99` (e.g. `+00000045.10`, `-00000120.00`); `TDATEnn ← MM/DD/YY` derived from `TRAN-ORIG-TS` (`YYYY-MM-DD-HH.MM.SS.ffffff`): `MM ← TS(6:2)`, `DD ← TS(9:2)`, `YY ← TS(3:2)`. Row 1 id → `TRNID-FIRST`, row 10 id → `TRNID-LAST`. |

## Browse primitives

| # | Given | Then |
|---|---|---|
| R-18 | `STARTBR` `NOTFND` | `TRANSACT-EOF`; `You are at the top of the page...`. Other error: `WS-ERR-FLG='Y'`, `Unable to lookup transaction...` (lower-case *t*). |
| R-19 | `READNEXT` `ENDFILE` | `TRANSACT-EOF`; `You have reached the bottom of the page...`. Other: `Unable to lookup transaction...`. |
| R-20 | `READPREV` `ENDFILE` | `TRANSACT-EOF`; `You have reached the top of the page...`. Other: `Unable to lookup transaction...`. |

## Paging

| # | Given | Then |
|---|---|---|
| R-21 | Forward/backward | Identical to COUSR00C R-18..R-23 with `TRAN-ID` as key and `CDEMO-CT00-*` fields (PF8 skip-read, 10-row fill, look-ahead read sets `NEXT-PAGE`, page counter increments only when ≥1 row filled). |

## RETURN-TO-PREV-SCREEN / SEND-TRNLST-SCREEN

| # | Given | Then |
|---|---|---|
| R-22 | Return | Blank target → `COSGN00C`; from-fields `CT00`/`COTRN00C`, context 0; `XCTL ... COMMAREA`. |
| R-23 | Send | Standard header (`CT00`, `COTRN00C`); `SEND MAP('COTRN0A') MAPSET('COTRN00') CURSOR` with `ERASE` unless `SEND-ERASE-NO`. |
