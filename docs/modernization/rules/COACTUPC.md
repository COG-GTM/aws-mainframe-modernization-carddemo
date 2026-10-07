# COACTUPC — Account update (transaction CAUP, map CACTUPA / mapset COACTUP)

Source: `app/cbl/COACTUPC.cbl` (3.3k lines; generic field editors driven by `WS-EDIT-VARIABLE-NAME`, date editing from
copybook `CSUTLDPY`, state/zip/area-code lookup tables from `CSLKPCDY`). Data access:
`CXACAIX` `READ` by account `X(11)` → `CVACT03Y`; `ACCTDAT` `READ`, then `READ … UPDATE` + `REWRITE` (`CVACT01Y`);
`CUSTDAT` `READ`, then `READ … UPDATE` + `REWRITE` by `CUST-ID X(9)` (`CVCUS01Y`); `SYNCPOINT` on exit,
`SYNCPOINT ROLLBACK` when the customer rewrite fails after the account rewrite.
Commarea: `CARDDEMO-COMMAREA` + `WS-THIS-PROGCOMMAREA` (`ACUP-CHANGE-ACTION` state: low-values/space = not fetched,
`S` show, `E` changes not OK, `N` validated/not confirmed, `C` committed, `L` lock error, `F` rewrite failed;
`ACUP-OLD-DETAILS` / `ACUP-NEW-DETAILS` = every account and customer field as split text).

## 0000-MAIN

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0`, or from `COMEN01C` and not re-enter | Commarea initialised; `CDEMO-PGM-ENTER`; `ACUP-DETAILS-NOT-FETCHED`. |
| R-2 | AID validity | ENTER, PF3; PF5 only in state `N`; PF12 only once details are fetched; anything else → ENTER. |
| R-3 | PF3 | To-tranid/program ← from-fields (blank → `CM00`/`COMEN01C`); from ← `CAUP`/`COACTUPC`; user type `U`; `CDEMO-PGM-ENTER`; last map ← `CACTUPA`/`COACTUP`; `EXEC CICS SYNCPOINT`; `XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA)`. |
| R-4 | Not fetched and `CDEMO-PGM-ENTER`, or from menu and not re-enter | Program commarea initialised; empty map sent with `Enter or update id of account to update`; `CDEMO-PGM-REENTER`; not fetched. |
| R-5 | State `C`, `L` or `F` and any key | Program commarea, misc storage and `CDEMO-ACCT-ID` cleared; fresh search map sent; state reset. |
| R-6 | Otherwise | `1000-PROCESS-INPUTS` (`RECEIVE MAP('CACTUPA') MAPSET('COACTUP')` into `ACUP-NEW-*`, `*`/spaces → low-values) → `1200-EDIT-MAP-INPUTS` → `2000-DECIDE-ACTION` → `3000-SEND-MAP`. |
| R-7 | `COMMON-RETURN` | `CCARD-ERROR-MSG ← WS-RETURN-MSG`; `RETURN TRANSID('CAUP') COMMAREA(WS-COMMAREA)`. |

## 1200-EDIT-MAP-INPUTS — search phase

| # | Given | Then |
|---|---|---|
| R-8 | `1210-EDIT-ACCOUNT`: `ACCTSID` blank | `INPUT-ERROR`; `FLG-ACCTFILTER-BLANK`; `Account number not provided`, overridden by `No input received`; `CDEMO-ACCT-ID ← 0`. |
| R-9 | Not numeric or zero | `INPUT-ERROR`; `Account Number if supplied must be a 11 digit Non-Zero Number`; `CDEMO-ACCT-ID ← 0`. Else `CDEMO-ACCT-ID ← CC-ACCT-ID`, `FLG-ACCTFILTER-ISVALID`. |

## 1200-EDIT-MAP-INPUTS — edit phase (details fetched)

`1205-COMPARE-OLD-NEW`: account group compared exactly except status (case-insensitive) and group id (trimmed,
case-insensitive); customer group compared trimmed/case-insensitive except phone parts, SSN, DOB, EFT id and FICO (exact).
Both groups unchanged → `NO-CHANGES-DETECTED` (`No change detected with respect to values fetched.`). No changes, or state
`N`/`C` → all non-key flags cleared, edits skipped. Otherwise state ← `E` and the edits below run **in this order**; the first
failing field supplies `WS-RETURN-MSG` (`IF WS-RETURN-MSG-OFF`), every failing field is flagged red. If no error, state ← `N`.
Messages are built as `<Field label><suffix>` with `FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)`.

| # | Field (label) | Editor | Rule / message |
|---|---|---|---|
| R-10 | `Account Status` | `1220-EDIT-YESNO` | blank → `Account Status must be supplied.`; not `Y`/`N` → `Account Status must be Y or N.` |
| R-11 | `Open Date` | `EDIT-DATE-CCYYMMDD` (`CSUTLDPY`) | year/month/day each mandatory (`Open Date : Year must be supplied.` etc.), `Open Date must be 4 digit number.`, `Open Date : Century is not valid.` (19/20 only), `Open Date: Month must be a number between 1 and 12.`, `Open Date:day must be a number between 1 and 31.`, `:Cannot have 30 days in this month.`, `:Cannot have 31 days in this month.`, `:Not a leap year.Cannot have 29 days in this month.`; finally `CSUTLDTC` (`YYYYMMDD`) must return severity 0000 else `<label> validation error Sev code: <sev> Message code: <msg>`. |
| R-12 | `Credit Limit` | `1250-EDIT-SIGNED-9V2` | blank → `Credit Limit must be supplied.`; `FUNCTION TEST-NUMVAL-C(x) ≠ 0` → `Credit Limit is not valid`. |
| R-13 | `Expiry Date` | date editor | as R-11 with label `Expiry Date`. |
| R-14 | `Cash Credit Limit` | signed 9V2 | as R-12. |
| R-15 | `Reissue Date` | date editor | as R-11. |
| R-16 | `Current Balance`, `Current Cycle Credit Limit`, `Current Cycle Debit Limit` | signed 9V2 | as R-12 (labels as listed). |
| R-17 | `SSN` | `1265-EDIT-US-SSN` → `1245-EDIT-NUM-REQD` ×3 | `SSN: First 3 chars` (3 digits) — blank `… must be supplied.`, non-numeric `… must be all numeric.`, zero `… must not be zero.`; then `000`, `666`, `900`–`999` → `SSN: First 3 chars: should not be 000, 666, or between 900 and 999`; `SSN 4th & 5th chars` (2), `SSN Last 4 chars` (4) numeric required. |
| R-18 | `Date of Birth` | date editor + `EDIT-DATE-OF-BIRTH` | as R-11 plus `Date of Birth:cannot be in the future ` (compared to `CURRENT-DATE`). |
| R-19 | `FICO Score` | `1245-EDIT-NUM-REQD`(3) then `1275` | numeric non-zero required; not 300..850 → `FICO Score: should be between 300 and 850`. |
| R-20 | `First Name` (25), `Last Name` (25) | `1225-EDIT-ALPHA-REQD` | blank → `<label> must be supplied.`; any char other than letters/space → `<label> can have alphabets only.` |
| R-21 | `Middle Name` (25) | `1235-EDIT-ALPHA-OPT` | optional; if present, letters/space only → `Middle Name can have alphabets only.` |
| R-22 | `Address Line 1` (50) | `1215-EDIT-MANDATORY` | blank → `Address Line 1 must be supplied.` (any content accepted). Address line 2 is not edited. |
| R-23 | `State` (2) | alpha required, then `1270-EDIT-US-STATE-CD` | not in `CSLKPCDY` `VALID-US-STATE-CODE` list → `State: is not a valid state code`. |
| R-24 | `Zip` (5) | `1245-EDIT-NUM-REQD` | `Zip must be supplied.` / `Zip must be all numeric.` / `Zip must not be zero.` |
| R-25 | `City` (50 = addr line 3) | alpha required | `City must be supplied.` / `City can have alphabets only.` |
| R-26 | `Country` (3) | alpha required | `Country must be supplied.` / `Country can have alphabets only.` |
| R-27 | `Phone Number 1`, `Phone Number 2` | `1260-EDIT-US-PHONE-NUM` | all three parts blank → valid (optional; note the source tests part A twice instead of C). Otherwise area code: blank `…: Area code must be supplied.`, non-numeric `…: Area code must be A 3 digit number.`, zero `…: Area code cannot be zero`, not in `CSLKPCDY` NANP list → `…: Not valid North America general purpose area code`; prefix: `…: Prefix code must be supplied.` / `…: Prefix code must be A 3 digit number.` / `…: Prefix code cannot be zero`; line: `…: Line number code must be supplied.` / `…: Line number code must be A 4 digit number.` / `…: Line number code cannot be zero`. |
| R-28 | `EFT Account Id` (10) | numeric required | `EFT Account Id must be supplied.` / `… must be all numeric.` / `… must not be zero.` |
| R-29 | `Primary Card Holder` | yes/no | `Primary Card Holder must be supplied.` / `Primary Card Holder must be Y or N.` |
| R-30 | State + Zip both valid | `1280-EDIT-US-STATE-ZIP-CD` | `state || zip(1:2)` not in `VALID-US-STATE-ZIP-CD2-COMBO` → both red, `Invalid zip code for state`. |

Not editable/edited: account id, customer id, group id (`ACUP-NEW-GROUP-ID` is only compared), government-issued id,
address line 2 (free text, carried through).

## 2000-DECIDE-ACTION

| # | Given | Then |
|---|---|---|
| R-31 | Not fetched, or PF12 (re-fetch) | If account id valid: `9000-READ-ACCT` (xref → account → customer reads as in `COACTVWC` R-10..R-14, same messages); customer found → `9500-STORE-FETCHED-DATA` (old values split into year/mon/day, phone parts, SSN parts; `CDEMO-ACCT-ID/CUST-ID/CARD-NUM/ACCT-STATUS/CUST-*NAME` set) and state `S`. |
| R-32 | State `S` + ENTER | error or no change → stay `S`; else state `N`. |
| R-33 | State `E` | stay (`Update account details presented above.`). |
| R-34 | State `N` + PF5 | `9600-WRITE-PROCESSING`; lock failure → `L`; rewrite failure → `F`; concurrent change → `S`; else `C`. |
| R-35 | State `N` + other key, or `C` | back to `S`; blank `CDEMO-FROM-TRANID` → `CDEMO-ACCT-ID`, `CDEMO-CARD-NUM` zeroed, `CDEMO-ACCT-STATUS` cleared. |
| R-36 | Other | `UNEXPECTED DATA SCENARIO`, abend 9999. |

## 9600-WRITE-PROCESSING / 9700-CHECK-CHANGE-IN-REC

| # | Given | Then |
|---|---|---|
| R-37 | `READ FILE('ACCTDAT ') UPDATE` not NORMAL | `Could not lock account record for update` (`L`). Then `READ FILE('CUSTDAT ') UPDATE RIDFLD(CDEMO-CUST-ID)` not NORMAL → `Could not lock customer record for update` (`L`). |
| R-38 | `9700`: account record differs from `ACUP-OLD-*` (status, 5 amounts, open/expiry/reissue date parts, group id case-insensitive) or customer record differs (names/address/country/govt id case-insensitive; zip, phones, SSN, DOB parts, EFT id, primary-holder, FICO exact) | `DATA-WAS-CHANGED-BEFORE-UPDATE`: `Record changed by some one else. Please review`; no rewrite. |
| R-39 | Unchanged | `ACCT-UPDATE-RECORD`: id, status, `CURR-BAL`, `CREDIT-LIMIT`, `CASH-CREDIT-LIMIT`, `CURR-CYC-CREDIT`, `CURR-CYC-DEBIT` from `NUMVAL-C` of the typed text; `OPEN-DATE`/`EXPIRAION-DATE`/`REISSUE-DATE` = `YYYY-MM-DD` from the split parts; `GROUP-ID`. `CUST-UPDATE-RECORD`: id, names, address 1–3, state, country, zip, phones formatted `(AAA)BBB-CCCC`, SSN (9 digits), govt id, DOB `YYYY-MM-DD`, EFT id, primary-holder, FICO. `REWRITE FILE('ACCTDAT ')` then `REWRITE FILE('CUSTDAT ')`. |
| R-40 | Account `REWRITE` fails | `Update of record failed` (`F`); customer rewrite still attempted. Customer `REWRITE` fails → `Update of record failed` and `EXEC CICS SYNCPOINT ROLLBACK` (account change undone). |

## 3250-SETUP-INFOMSG

| # | Given | Then |
|---|---|---|
| R-41 | Info text by state | enter/not fetched → `Enter or update id of account to update`; `S`/`E` → `Update account details presented above.`; `N` → `Changes validated.Press F5 to save`; `C` → `Changes committed to database`; `L`/`F` → `Changes unsuccessful. Please try again`. `ERRMSGO ← WS-RETURN-MSG`. `SEND MAP('CACTUPA') MAPSET('COACTUP') CURSOR ERASE FREEKB`. |

## Java port (`PUT /api/v1/accounts/{id}`)

`com.carddemo.web.account.AccountController#update` → `com.carddemo.account.online.AccountUpdateService` (one
`@Transactional` unit), edits in `AccountUpdateEdits` (paragraph order of `1200-EDIT-MAP-INPUTS`), lookup tables
`com.carddemo.common.online.Cslkpcdy` (read from `CSLKPCDY.cpy` on the classpath), amounts `common.codec.NumvalC`.
Tests: `com.carddemo.web.AccountUpdateRulesTest` (one test per R-id; R-10..R-30 parameterized, one row per message),
`OnlineApiIT`.

- Dialogue: the request body is the `CACTUPA` input (dates/SSN/phones split as on the map) plus `accountVersion`,
  `customerVersion` and `confirm`. Clients start from `updateForm` of `GET /api/v1/accounts/{id}` (= R-31 fetch, state
  `S`). `confirm=false` is ENTER: edits only, response state `VALIDATED` (`N`, `Changes validated.Press F5 to save`),
  nothing written. `confirm=true` is ENTER + PF5 in one request: edits, then rewrite → `COMMITTED` (`C`). No changes →
  200 state `SHOW` with `No change detected with respect to values fetched.` (R-32, case/trailing-space-insensitive as
  `9700`/`1205`).
- Edit errors → 400 `INVREQ`; `field`/`message` = the first failing edit (what `ERRMSG` shows), `invalidFields` = every
  field the program would turn red, in edit order. R-30 reports `zip` with `invalidFields` `[zip, state]`.
- Protected map fields (group id, government id, address line 2) are carried through to the rewrite as typed; country is
  not on the request (protected) but is still edited from the stored value (R-26).
- R-37/R-38: no `READ … UPDATE` lock across requests (ADR-0010): the supplied versions are compared with the current
  rows. On `confirm=true` both rows are then locked for the rest of the transaction (`AccountRepository.lockVersion` /
  `CustomerRepository.lockVersion`, `SELECT version … FOR UPDATE`, the `READ … UPDATE` of `9600`) and their committed
  versions compared again, so a concurrent change to the record the request leaves unchanged (which the JPA `@Version`
  check at flush does not cover) is still caught; any mismatch → 409 `CHANGED`
  `Record changed by some one else. Please review`, nothing written. Row missing or lock not obtained → 500 `ABEND`
  `Could not lock account record for update` / `Could not lock customer record for update`.
- R-27: the optional-phone test compares part A twice (`A = SPACES OR C = LOW-VALUES`); because `1100-RECEIVE-MAP`
  stores blank parts as LOW-VALUES, that clause is "C blank", so the phone is optional only when all three parts are
  blank (a line number alone goes through the part edits, as in the COBOL).
- R-40: a failed rewrite → 500 `ABEND` `Update of record failed`; the transaction rolls back both rows (the
  `SYNCPOINT ROLLBACK`). State `L`/`F` (`Changes unsuccessful. Please try again`) is therefore never returned with 200.
- R-36: malformed JSON / missing versions → 400 `INVREQ`.

