# COCRDLIC — Credit card list (transaction CCLI, map CCRDLIA / mapset COCRDLI)

Source: `app/cbl/COCRDLIC.cbl`. Data access: `CARDDAT` KSDS browsed by `WS-CARD-RID-CARDNUM X(16)`
(`STARTBR … GTEQ`, `READNEXT`, `READPREV`, `ENDBR`; record `CVACT02Y`: `CARD-NUM`, `CARD-ACCT-ID`, `CARD-ACTIVE-STATUS`).
`CARDAIX` is declared (`LIT-CARD-FILE-ACCT-PATH`) but never used — account filtering is done by scanning `CARDDAT`.
Page size `WS-MAX-SCREEN-LINES` = 7 rows (`CRDSEL1..7`, `WS-ROW-ACCTNO`, `WS-ROW-CARD-NUM`, `WS-ROW-CARD-STATUS`).
Commarea: `CARDDEMO-COMMAREA` + `WS-THIS-PROGCOMMAREA` (`WS-CA-LAST-CARDKEY`/`WS-CA-FIRST-CARDKEY` = card num + acct id,
`WS-CA-SCREEN-NUM 9(1)`, `WS-CA-LAST-PAGE-DISPLAYED` (0 shown / 9 not shown), `WS-CA-NEXT-PAGE-IND` ('Y'/low-values),
the 7 row keys), returned as `WS-COMMAREA X(2000)`. Valid AIDs: ENTER, PF3, PF7, PF8 — anything else is treated as ENTER.

## 0000-MAIN

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | Commarea initialised; from ← `CCLI`/`COCRDLIC`; `CDEMO-USRTYP-USER`; `CDEMO-PGM-ENTER`; `CA-FIRST-PAGE` (`SCREEN-NUM=1`); `CA-LAST-PAGE-NOT-SHOWN`. |
| R-2 | `CDEMO-PGM-ENTER` and `CDEMO-FROM-PROGRAM ≠ 'COCRDLIC'` (arrived from the menu or back from detail/update) | `WS-THIS-PROGCOMMAREA` initialised; first page; last page not shown. The `CDEMO-ACCT-ID`/`CDEMO-CARD-NUM` carried in the commarea are **not** used as filters (only typed filters are, R-10/R-11). |
| R-3 | `EIBCALEN > 0` and from `COCRDLIC` | `RECEIVE MAP('CCRDLIA') MAPSET('COCRDLI')` and `2200-EDIT-INPUTS` before dispatch. |
| R-4 | PF3 and from `COCRDLIC` | From ← `CCLI`/`COCRDLIC`; user type forced to `U`; `CDEMO-PGM-ENTER`; last map/mapset ← `CCRDLIA`/`COCRDLI`; `WS-EXIT-MESSAGE='PF03 PRESSED.EXITING'`; `XCTL PROGRAM('COMEN01C') COMMAREA(CARDDEMO-COMMAREA)`. |
| R-5 | Any AID other than PF8 | `CA-LAST-PAGE-NOT-SHOWN` reset. |
| R-6 | Dispatch — `INPUT-ERROR` | Error message to `CCARD-ERROR-MSG`; if neither filter is NOT-OK (i.e. the error is in the selection column) the current page is re-read forward from `WS-CARD-RID-CARDNUM` (low-values → first page) and re-sent; otherwise the page is re-sent with no data rows (rows protected). |
| R-7 | PF7 on `CA-FIRST-PAGE` | Re-read forward from `WS-CA-FIRST-CARD-NUM`; message `NO PREVIOUS PAGES TO DISPLAY` (set in `1400-SETUP-MESSAGE`). |
| R-8 | `CDEMO-PGM-REENTER` and from another program | Treated as a fresh start: commarea initialised, first page, read forward from `WS-CA-FIRST-CARD-NUM` (low-values). |
| R-9 | PF8 and `CA-NEXT-PAGE-EXISTS` | `WS-CARD-RID-CARDNUM ← WS-CA-LAST-CARD-NUM`; `SCREEN-NUM + 1`; `9000-READ-FORWARD` (the previous page's last key is re-read as row 1 of the new page — see R-18). PF8 without a next page → `NO MORE PAGES TO DISPLAY` once `CA-LAST-PAGE-SHOWN`, else the page is re-read and `TYPE S FOR DETAIL, U TO UPDATE ANY RECORD` is shown and `CA-LAST-PAGE-SHOWN` set. |
| R-10 | PF7 and not first page | `WS-CARD-RID-CARDNUM ← WS-CA-FIRST-CARD-NUM`; `SCREEN-NUM - 1`; `9100-READ-BACKWARDS`. |
| R-11 | ENTER, exactly one row with `S` | From ← `CCLI`/`COCRDLIC`; user type `U`; `CDEMO-PGM-ENTER`; `CDEMO-ACCT-ID ← WS-ROW-ACCTNO(I-SELECTED)`, `CDEMO-CARD-NUM ← WS-ROW-CARD-NUM(I-SELECTED)`; next map `CCRDSLA`/`COCRDSL`; `XCTL PROGRAM('COCRDSLC') COMMAREA(CARDDEMO-COMMAREA)`. |
| R-12 | ENTER, exactly one row with `U` | Same, but next `CCRDUPA`/`COCRDUP`; `XCTL PROGRAM('COCRDUPC')`. |
| R-13 | Otherwise (ENTER with no selection / filters changed) | Read forward from `WS-CA-FIRST-CARD-NUM` and send. |
| R-14 | `COMMON-RETURN` | From ← `CCLI`/`COCRDLIC`, last map ← `CCRDLIA`/`COCRDLI`; `RETURN TRANSID('CCLI') COMMAREA(WS-COMMAREA)`. |

## 2200-EDIT-INPUTS

| # | Given | Then |
|---|---|---|
| R-15 | `2210-EDIT-ACCOUNT`: `ACCTSIDI` low-values/spaces/zero | `FLG-ACCTFILTER-BLANK`; `CDEMO-ACCT-ID ← 0` (no filter). Not numeric → `INPUT-ERROR`, `FLG-ACCTFILTER-NOT-OK`, select rows protected, `WS-ERROR-MSG='ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'`, `CDEMO-ACCT-ID ← 0`. Numeric → `CDEMO-ACCT-ID ← CC-ACCT-ID`, `FLG-ACCTFILTER-ISVALID`. |
| R-16 | `2220-EDIT-CARD`: likewise for `CARDSIDI` | Not numeric → `INPUT-ERROR`, `FLG-CARDFILTER-NOT-OK`, message (only if none yet) `CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER`. Numeric → `CDEMO-CARD-NUM ← CC-CARD-NUM-N`, `FLG-CARDFILTER-ISVALID`. |
| R-17 | `2250-EDIT-ARRAY` (skipped when a filter failed): count of `S`+`U` in `CRDSEL1..7` > 1 | `INPUT-ERROR`; `WS-ERROR-MSG='PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE'`; every selected row flagged `'1'` (shown red). A row value other than `S`, `U`, space or low-values → `INPUT-ERROR`, row red, message (if none yet) `INVALID ACTION CODE`. Lower-case `s`/`u` are **invalid**. `I-SELECTED` = last row index holding `S`/`U`. |

## 9000-READ-FORWARD (page build)

| # | Given | Then |
|---|---|---|
| R-18 | Start | Rows cleared; `STARTBR DATASET('CARDDAT ') RIDFLD(WS-CARD-RID-CARDNUM) GTEQ`; `CA-NEXT-PAGE-EXISTS` assumed. Loop `READNEXT` until exit. |
| R-19 | Record read (NORMAL/DUPREC) | `9500-FILTER-RECORDS`: when `FLG-ACCTFILTER-ISVALID`, keep only `CARD-ACCT-ID = CC-ACCT-ID`; then, when `FLG-CARDFILTER-ISVALID`, also keep only `CARD-NUM = CC-CARD-NUM-N`; no valid filter keeps all. Both filters apply when both are valid (corrected from source in UNT51-19: the two `IF`s are sequential, the earlier text said the account filter takes precedence). Kept record → row `n`: card num, acct id, active status; row 1 also sets `WS-CA-FIRST-CARDKEY` and bumps `SCREEN-NUM` from 0 to 1. |
| R-20 | 7 rows filled | `WS-CA-LAST-CARDKEY ← row 7 key`; one look-ahead `READNEXT`: NORMAL/DUPREC → `CA-NEXT-PAGE-EXISTS` and `WS-CA-LAST-CARDKEY ← look-ahead key` (that record becomes row 1 of the next page); ENDFILE → `CA-NEXT-PAGE-NOT-EXISTS`, message (if none) `NO MORE RECORDS TO SHOW`; other → file error message. Look-ahead is not filtered. |
| R-21 | `ENDFILE` before 7 rows | `CA-NEXT-PAGE-NOT-EXISTS`; last key ← last record read; message (if none) `NO MORE RECORDS TO SHOW`; if `SCREEN-NUM = 1` and no rows → `WS-NO-RECORDS-FOUND` (`NO RECORDS FOUND FOR THIS SEARCH CONDITION.` — set as the info message, but suppressed in `1400-SETUP-MESSAGE`). |
| R-22 | Other RESP | Loop exits; `WS-ERROR-MSG ← WS-FILE-ERROR-MESSAGE`. Always `ENDBR`. |

## 9100-READ-BACKWARDS

| # | Given | Then |
|---|---|---|
| R-23 | PF7 | `WS-CA-LAST-CARDKEY ← WS-CA-FIRST-CARDKEY`; `STARTBR GTEQ` at the first key of the current page; one `READPREV` positions on it (counter = 8 → 7), then `READPREV` repeatedly, filling rows 7,6,…,1 with filtered records; when row 1 is filled `WS-CA-FIRST-CARDKEY ← that record`. Any non-NORMAL/DUPREC RESP (incl. ENDFILE when fewer than 7 earlier records) exits with `WS-ERROR-MSG ← WS-FILE-ERROR-MESSAGE`; upper rows stay low-values. `ENDBR`. |

## 1000-SEND-MAP

| # | Given | Then |
|---|---|---|
| R-24 | `1100-SCREEN-INIT` | `PAGENOO ← WS-CA-SCREEN-NUM`; info message dark. |
| R-25 | `1200/1250` rows | Each row shows acct id, card num, status; a row whose data is low-values, or all rows when `FLG-PROTECT-SELECT-ROWS-YES` (filter error), is protected (`DFHBMPRF` row 1 / `DFHBMPRO` others); `WS-ROW-CRDSELECT-ERROR(n)='1'` → red; otherwise unprotected `DFHBMFSE`. |
| R-26 | `1300-SETUP-SCREEN-ATTRS` | Filter fields re-displayed from `CC-ACCT-ID`/`CC-CARD-NUM` (valid or NOT-OK) or `CDEMO-*`; NOT-OK → red with cursor; `INPUT-OK` → cursor on `ACCTSID`. |
| R-27 | `1400-SETUP-MESSAGE` | Filter errors keep their message; PF7 on first page → `NO PREVIOUS PAGES TO DISPLAY`; PF8 with no next page: `CA-LAST-PAGE-SHOWN` → `NO MORE PAGES TO DISPLAY`, else info `TYPE S FOR DETAIL, U TO UPDATE ANY RECORD` and `CA-LAST-PAGE-SHOWN`; `CA-NEXT-PAGE-EXISTS` → same info text. `ERRMSGO ← WS-ERROR-MSG`; info shown neutral unless blank or `NO RECORDS FOUND…`. |
| R-28 | `1500-SEND-SCREEN` | `SEND MAP('CCRDLIA') MAPSET('COCRDLI') CURSOR ERASE FREEKB`. |

## Java port notes (UNT51-19, `GET /api/v1/cards`, `POST /api/v1/cards/selection`)

- **User restriction.** The header comment promises "all cards if no context passed and admin user; only the ones
  associated with ACCT in COMMAREA if user is not admin", but no paragraph tests `CDEMO-USRTYP-*` (every `XCTL` even
  forces `CDEMO-USRTYP-USER`) and `CDEMO-ACCT-ID` is never used as a filter (R-2): in the COBOL every user sees every
  card. The port implements the documented intent (ADR-0020): an ADMIN without an account sees all cards; a USER must
  name the account in context (`accountId`, 403 `NOTAUTH` otherwise) and only sees its cards.
- **Look-ahead (R-20).** The ADR-0011 keyset page filters its look-ahead record, so `hasNextPage` is false when no
  further card matches the filter; COBOL's unfiltered look-ahead would offer a PF8 that leads to an empty page.
- **PF7 short of seven earlier rows (R-23).** COBOL leaves the upper rows empty and shows the `ENDFILE` file-error
  message; the port returns the earlier rows (top-aligned) without an error. With a full first page (the normal case,
  and every unfiltered page of the sample data) both show the same seven rows.
- **PF8 with no next page (R-9).** Stateless: `after` = the last card of the last page answers an empty page with
  `NO MORE PAGES TO DISPLAY` (the client is told `nextPage = null` beforehand).
- **Selection (R-11, R-12, R-17).** `POST /api/v1/cards/selection` takes the `CRDSEL` codes of the rows shown and
  returns the `XCTL` target (`COCRDSLC` for `S`, `COCRDUPC` for `U`) with `acctId` and the card's `cardRef` instead of
  `CDEMO-CARD-NUM` (ADR-0020).
- **Masking.** List rows show `************nnnn` instead of `CRDNUMn` (ADR-0020).
