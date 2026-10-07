# COCRDUPC — Credit card update (transaction CCUP, map CCRDUPA / mapset COCRDUP)

Source: `app/cbl/COCRDUPC.cbl`. Data access: `CARDDAT` KSDS by `WS-CARD-RID-CARDNUM X(16)` — `READ` (fetch),
`READ … UPDATE` + `REWRITE` (save), `SYNCPOINT` on exit. Record `CVACT02Y` (`CARD-NUM`, `CARD-ACCT-ID`, `CARD-CVV-CD`,
`CARD-EMBOSSED-NAME X(50)`, `CARD-EXPIRAION-DATE X(10) YYYY-MM-DD`, `CARD-ACTIVE-STATUS X`).
Commarea: `CARDDEMO-COMMAREA` + `WS-THIS-PROGCOMMAREA` (`CCUP-CHANGE-ACTION` state: low-values/space = details not
fetched, `S` show details, `E` changes not OK, `N` changes OK not confirmed, `C` committed, `L` lock error, `F` rewrite
failed; `CCUP-OLD-DETAILS` and `CCUP-NEW-DETAILS` = acct id, card id, CVV, name, expiry day/month/year, status).
Updatable fields: `CRDNAME`, `CRDSTCD`, `EXPMON`, `EXPYEAR` (`EXPDAY` is carried but protected). Account id / card number
are search keys only — the key cannot be changed.

## 0000-MAIN

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0`, or from `COMEN01C` and not re-enter | Commarea initialised; `CDEMO-PGM-ENTER`; `CCUP-DETAILS-NOT-FETCHED`. |
| R-2 | AID validity | ENTER, PF3 always; PF5 only when `CCUP-CHANGES-OK-NOT-CONFIRMED` (`N`); PF12 only when details have been fetched. Any other AID (or PF5/PF12 out of state) is treated as ENTER. |
| R-3 | PF3, or state `C`/`L`/`F` when `CDEMO-LAST-MAPSET='COCRDLI'` (came from the list) | Return to caller: `CDEMO-TO-TRANID/PROGRAM ← CDEMO-FROM-*` (blank → `CM00`/`COMEN01C`); from ← `CCUP`/`COCRDUPC`; if from the list, `CDEMO-ACCT-ID` and `CDEMO-CARD-NUM` zeroed; user type `U`; `CDEMO-PGM-ENTER`; last map ← `CCRDUPA`/`COCRDUP`; `EXEC CICS SYNCPOINT`; `XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)`. |
| R-4 | `CDEMO-PGM-ENTER` from `COCRDLIC`, or PF12 while from `COCRDLIC` | Keys taken from commarea (`CC-ACCT-ID-N ← CDEMO-ACCT-ID`, `CC-CARD-NUM-N ← CDEMO-CARD-NUM`), both filters valid; `9000-READ-DATA`; `CCUP-SHOW-DETAILS`; send map. |
| R-5 | Details not fetched and `CDEMO-PGM-ENTER`, or from menu and not re-enter | `WS-THIS-PROGCOMMAREA` initialised; empty search map sent; `CDEMO-PGM-REENTER`; `CCUP-DETAILS-NOT-FETCHED`. |
| R-6 | State `C`, `L` or `F` (after the confirmation screen was shown) and any key | Program commarea, `CDEMO-ACCT-ID`, `CDEMO-CARD-NUM` cleared; fresh empty map sent (`PROMPT-FOR-SEARCH-KEYS`); state reset to not fetched. |
| R-7 | Otherwise | `1000-PROCESS-INPUTS` → `2000-DECIDE-ACTION` → `3000-SEND-MAP`. |
| R-8 | `COMMON-RETURN` | `CCARD-ERROR-MSG ← WS-RETURN-MSG`; `RETURN TRANSID('CCUP') COMMAREA(WS-COMMAREA)`. |

## 1100-RECEIVE-MAP / 1200-EDIT-MAP-INPUTS

| # | Given | Then |
|---|---|---|
| R-9 | `RECEIVE MAP('CCRDUPA') MAPSET('COCRDUP')` | Each of `ACCTSID`, `CARDSID`, `CRDNAME`, `CRDSTCD`, `EXPMON`, `EXPYEAR` equal to `*` or spaces → low-values in `CCUP-NEW-*`; otherwise the typed value. `CCUP-NEW-EXPDAY ← EXPDAYI` unconditionally. |
| R-10 | `CCUP-DETAILS-NOT-FETCHED` (search phase) | `1210-EDIT-ACCOUNT` then `1220-EDIT-CARD`; new card data cleared; both blank → `No input received`. |
| R-11 | `1210`: account blank/zero | `INPUT-ERROR`, `FLG-ACCTFILTER-BLANK`, message (if none) `Account number not provided`, `CDEMO-ACCT-ID ← 0`. Not numeric → `FLG-ACCTFILTER-NOT-OK`, `ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER`. Else `CDEMO-ACCT-ID ← CC-ACCT-ID`, valid. |
| R-12 | `1220`: card blank/zero | `Card number not provided`; not numeric → `CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER`; else `CDEMO-CARD-NUM ← CC-CARD-NUM-N`, valid. |
| R-13 | Details fetched (edit phase) | Keys restored from `CCUP-OLD-*`; `FUNCTION UPPER-CASE(CCUP-NEW-CARDDATA) = UPPER-CASE(CCUP-OLD-CARDDATA)` → `NO-CHANGES-DETECTED` (`No change detected with respect to values fetched.`). No changes, or state `N`/`C` → all field flags valid, skip edits. |
| R-14 | Changes present | State ← `E`; `1230`–`1260` run; if no `INPUT-ERROR` state ← `N` (`CCUP-CHANGES-OK-NOT-CONFIRMED`). |
| R-15 | `1230-EDIT-NAME`: blank | `INPUT-ERROR`; `Card name not provided`. Name containing anything other than A–Z, a–z and space (checked by converting letters to spaces and trimming) → `Card name can only contain alphabets and spaces`. |
| R-16 | `1240-EDIT-CARDSTATUS` | Blank, or not `Y`/`N` (upper case only) → `Card Active Status must be Y or N`. |
| R-17 | `1250-EDIT-EXPIRY-MON` | Blank/zero, or not 1..12 (`VALID-MONTH`) → `Card expiry month must be between 1 and 12`. |
| R-18 | `1260-EDIT-EXPIRY-YEAR` | Blank/zero, or not 1950..2099 (`VALID-YEAR`) → `Invalid card expiry year`. |

First error message wins (`IF WS-RETURN-MSG-OFF`); every failing field is coloured red and the cursor goes to the first.

## 2000-DECIDE-ACTION

| # | Given | Then |
|---|---|---|
| R-19 | Details not fetched (or PF12 → re-fetch) | If both keys valid: `9000-READ-DATA`; found → state `S` (`Details of selected card shown above`), fields unprotected. |
| R-20 | State `S` and ENTER | Input error or no changes → stay `S`; else state `N`, info `Changes validated.Press F5 to save`. |
| R-21 | State `E` | Stay; info `Update card details presented above.` with the validation message in `ERRMSG`. |
| R-22 | State `N` and PF5 | `9200-WRITE-PROCESSING`; `COULD-NOT-LOCK-FOR-UPDATE` → state `L`; `LOCKED-BUT-UPDATE-FAILED` → `F`; `DATA-WAS-CHANGED-BEFORE-UPDATE` → back to `S` (fresh values shown, message `Record changed by some one else. Please review`); else `C` (`Changes committed to database`). |
| R-23 | State `N` and ENTER (not PF5), or state `C` | Back to `S` (edit again); if `CDEMO-FROM-TRANID` blank, `CDEMO-ACCT-ID`/`CDEMO-CARD-NUM` zeroed and `CDEMO-ACCT-STATUS` cleared. |
| R-24 | Any other state | `ABEND-CODE='0001'`, `ABEND-MSG='UNEXPECTED DATA SCENARIO'`, `ABEND-ROUTINE` (ABCODE 9999). |

## 9000-READ-DATA / 9100-GETCARD-BYACCTCARD

| # | Given | Then |
|---|---|---|
| R-25 | `READ FILE('CARDDAT ') RIDFLD(CC-CARD-NUM)` NORMAL | `FOUND-CARDS-FOR-ACCOUNT`; `CCUP-OLD-*` ← CVV, embossed name **upper-cased**, expiry year `(1:4)`, month `(6:2)`, day `(9:2)`, status; `CCUP-OLD-ACCTID/CARDID` ← typed keys (the record's `CARD-ACCT-ID` is **not** compared with the typed account). |
| R-26 | `NOTFND` | `INPUT-ERROR`; both filters NOT-OK; `Did not find cards for this search condition`. |
| R-27 | Other RESP | `INPUT-ERROR`; `FLG-ACCTFILTER-NOT-OK`; `WS-FILE-ERROR-MESSAGE` (`File Error: READ on CARDDAT …`). |

## 9200-WRITE-PROCESSING / 9300-CHECK-CHANGE-IN-REC

| # | Given | Then |
|---|---|---|
| R-28 | `READ FILE('CARDDAT ') UPDATE RIDFLD(CC-CARD-NUM)` not NORMAL | `INPUT-ERROR`; `Could not lock record for update` (`L`). |
| R-29 | Record re-read differs from `CCUP-OLD-*` (CVV, upper-cased name, year, month, day, status) | `DATA-WAS-CHANGED-BEFORE-UPDATE`; `CCUP-OLD-*` refreshed from the current record; no rewrite (`Record changed by some one else. Please review`). |
| R-30 | Unchanged | `CARD-UPDATE-RECORD` built: `CARD-NUM ← CCUP-NEW-CARDID`, `CARD-ACCT-ID ← CC-ACCT-ID-N`, `CARD-CVV-CD ← CCUP-NEW-CVV-CD` (numeric), `CARD-EMBOSSED-NAME ← CCUP-NEW-CRDNAME` (as typed, not upper-cased), `CARD-EXPIRAION-DATE ← YYYY-MM-DD` from `CCUP-NEW-EXPYEAR/EXPMON/EXPDAY`, `CARD-ACTIVE-STATUS ← CCUP-NEW-CRDSTCD`; filler 59 bytes low-values/spaces per `INITIALIZE`. `REWRITE FILE('CARDDAT ')`. |
| R-31 | `REWRITE` not NORMAL | `LOCKED-BUT-UPDATE-FAILED` → `Update of record failed` (`F`); info `Changes unsuccessful. Please try again`. |

## 3250-SETUP-INFOMSG

| # | Given | Then |
|---|---|---|
| R-32 | Info text by state | enter / not fetched → `Please enter Account and Card Number`; `S` → `Details of selected card shown above`; `E` → `Update card details presented above.`; `N` → `Changes validated.Press F5 to save`; `C` → `Changes committed to database`; `L`/`F` → `Changes unsuccessful. Please try again`. `INFOMSGO ← WS-INFO-MSG`, `ERRMSGO ← WS-RETURN-MSG`. |
| R-33 | Attributes (`3300`) | Search keys protected once details are fetched; editable fields protected except in states `S`/`E`; `EXPDAY` always protected; PF5 only honoured in state `N`. `SEND MAP('CCRDUPA') MAPSET('COCRDUP') CURSOR ERASE FREEKB`. |

## Java port notes (UNT51-19, `PUT /api/v1/cards/{cardNumber}`)

- One request runs the dialogue: keys (R-10..R-12) → read (R-25..R-27) → version check (ADR-0010; replaces the
  R-29 field-by-field comparison of `CCUP-OLD-*`, 409 `CHANGED`) → change detection (R-13) → edits 1230..1260
  (R-14..R-18) → `confirm=false` = ENTER (state `N`, nothing written) or `confirm=true` = PF5 (`lockVersion`
  `SELECT ... FOR UPDATE` = `READ UPDATE`, version re-check, `REWRITE`).
- R-30: `CARD-ACCT-ID` keeps the stored account (COBOL moves the typed search key, which is protected once details
  are fetched). Name as typed, day of the expiry date kept, CVV unchanged. Deliberate
  deviation: the kept day is clamped to the last day of the new month (31 → 28/29 for February, 30 for April);
  COBOL would REWRITE an impossible date such as `2024-02-31`, which the generated `expiration_date_dt` DATE column
  cannot represent (it would become NULL and drop the card from date queries).
- R-25 stays for an ADMIN (typed account not cross-checked); a USER can only update a card of the account given,
  else NOTFND (ADR-0020).
- A one-digit month is accepted by the edit (COBOL `NUMVAL`-style 1..12) and stored as `MM`.
