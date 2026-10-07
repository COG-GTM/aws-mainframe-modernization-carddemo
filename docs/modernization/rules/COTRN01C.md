# COTRN01C — Transaction view (transaction CT01, map COTRN1A / mapset COTRN01)

Source: `app/cbl/COTRN01C.cbl`. Data access: `TRANSACT` KSDS (`WS-TRANSACT-FILE = 'TRANSACT'`), `READ ... UPDATE`
by full key `TRAN-ID X(16)` (record `CVTRA05Y`). Commarea extension `CDEMO-CT01-TRN-SELECTED X(16)` (set by COTRN00C
when a list row is selected with `S`).

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry, `CDEMO-CT01-TRN-SELECTED` blank | Clear map, cursor `TRNIDIN`, send. |
| R-3 | First entry, `CDEMO-CT01-TRN-SELECTED` non-blank | Copied to `TRNIDINI`; `PROCESS-ENTER-KEY` runs (record shown immediately). |
| R-4 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY`. |
| R-5 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM` = `CDEMO-FROM-PROGRAM` if non-blank else `COMEN01C`; `RETURN-TO-PREV-SCREEN`. |
| R-6 | Re-entry + `DFHPF4` | `CLEAR-CURRENT-SCREEN`: `TRNIDIN` and all 13 detail fields + message blank, cursor `TRNIDIN`. |
| R-7 | Re-entry + `DFHPF5` | `CDEMO-TO-PROGRAM='COTRN00C'` (back to the list); `RETURN-TO-PREV-SCREEN`. |
| R-8 | Other AID | `Invalid key pressed. Please see below...`; re-send. Non-XCTL paths `RETURN TRANSID('CT01') COMMAREA(...)`. |

## PROCESS-ENTER-KEY

| # | Given | Then |
|---|---|---|
| R-9 | `TRNIDINI` spaces/low-values | `WS-ERR-FLG='Y'`; `Tran ID can NOT be empty...`; cursor `TRNIDIN`. |
| R-10 | Present | Detail fields blanked; `TRAN-ID ← TRNIDINI` (no numeric check, no padding beyond the 16-byte move); `READ-TRANSACT-FILE`. |
| R-11 | Read OK | Screen fields: `TRNID←TRAN-ID`, `CARDNUM←TRAN-CARD-NUM`, `TTYPCD←TRAN-TYPE-CD`, `TCATCD←TRAN-CAT-CD`, `TRNSRC←TRAN-SOURCE`, `TRNAMT←WS-TRAN-AMT` (edited `PIC +99999999.99` of `TRAN-AMT S9(9)V99`, e.g. `+00000183.88`), `TDESC←TRAN-DESC`, `TORIGDT←TRAN-ORIG-TS`, `TPROCDT←TRAN-PROC-TS`, `MID←TRAN-MERCHANT-ID`, `MNAME←TRAN-MERCHANT-NAME`, `MCITY←TRAN-MERCHANT-CITY`, `MZIP←TRAN-MERCHANT-ZIP`; screen sent. |

## READ-TRANSACT-FILE (`READ DATASET('TRANSACT') ... UPDATE`)

| # | Given | Then |
|---|---|---|
| R-12 | `NORMAL` | Continue (R-11). The `UPDATE` lock is released at task end; nothing is rewritten. |
| R-13 | `NOTFND` | `WS-ERR-FLG='Y'`; `Transaction ID NOT found...`; cursor `TRNIDIN`. |
| R-14 | Other | `DISPLAY 'RESP:'...`; `WS-ERR-FLG='Y'`; `Unable to lookup Transaction...`; cursor `TRNIDIN`. |

## RETURN-TO-PREV-SCREEN / SEND

| # | Given | Then |
|---|---|---|
| R-15 | Return | Blank target → `COSGN00C`; `CDEMO-FROM-TRANID='CT01'`, `CDEMO-FROM-PROGRAM='COTRN01C'`, context 0; `XCTL ... COMMAREA`. |
| R-16 | Send | Standard header (`CT01`, `COTRN01C`); `SEND MAP('COTRN1A') MAPSET('COTRN01') ERASE CURSOR`. |

## Java port notes (UNT51-20, `GET /api/v1/transactions/{tranId}`)

- The id is looked up as typed after right-trim (R-10: no numeric check, no zero padding); blank is 400, NOTFND 404,
  other errors 500 `ABEND`. Nothing is read for update (the COBOL `READ UPDATE` lock is never used).
- `exit` = PF3 (`fromProgram` query parameter, else COMEN01C), `list` = PF5 (COTRN00C). PF4 is a client-side clear.
- COTRN1A shows the full card number, so the detail returns it (ADR-0020: full PAN only where the map shows it).
- Role: no ownership check — COTRN01C has none (ADR-0020 §4).
