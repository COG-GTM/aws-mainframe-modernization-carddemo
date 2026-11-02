# card-service vs COCRDLIC / COCRDSLC / COCRDUPC

Paragraph-by-paragraph review of `modernization/card-service` against the three CICS card programs
(UNT149-18). "Fact" comes from reading `app/cbl` and `app/bms`; "Hypothesis" is labelled. Online
programs have no CICS runtime here, so each row is covered by a source-derived scenario in
`CardControllerTest` (scope §4).

## COCRDLIC (CCLI) → `GET /api/v1/cards`

| COBOL (fact) | Before | Now |
|---|---|---|
| 2210-EDIT-ACCOUNT: blank, spaces or all zeros = no filter; not numeric → `ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER` (field `ACCTSID`, length 11) | `Long accountId`, generic 400 | `accountId` must be 11 digits, empty / all zeros = no filter, COBOL message in `fieldErrors` |
| 2220-EDIT-CARD: same rules for the card number, `CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER` | **missing** | `cardNumber` filter added with the same rules |
| 9500-FILTER-RECORDS drops records whose account or card number differ from the valid filters → both filters intersect | intersected account with customer only | account, card number and (extension) customer filters all intersect |
| 9000-READ-FORWARD / 9100-READ-BACKWARDS browse CARDDAT by `CARD-NUM`, 7 rows (`WS-MAX-SCREEN-LINES`) | sort by `cardNum` | unchanged; `size=7` pages tested across a page boundary (PF8 = `page+1`, PF7 = `page-1`) |
| Header comment says admin sees all cards and a user only the COMMAREA account's; **the code never branches on user type** (it sets `CDEMO-USRTYP-USER` itself, the account filter is optional) and `CSUSR01Y` has no account id | no role filter | no role filter, as recorded in api-style.md §5; a `U` token sees the same rows as an `A` token (tested). The ticket's "user sees only cards of their account" is not implementable from the source: there is no account to restrict to. Needs a product decision if wanted. |
| `NO RECORDS FOUND FOR THIS SEARCH CONDITION.`, `NO MORE PAGES TO DISPLAY`, `NO PREVIOUS PAGES TO DISPLAY` | – | empty `content` / `totalPages` (api-style.md §4); no message text |
| 2250-EDIT-ARRAY: one `S` (view) or `U` (update) per screen | – | client navigation to `GET` / `PUT /api/v1/cards/{n}`; nothing to port |
| `customerId` | present | kept as an extension (not on the screen), through CARDXREF |

## COCRDSLC (CCDL) → `GET /api/v1/cards/{cardNumber}`

| COBOL (fact) | Before | Now |
|---|---|---|
| 2220-EDIT-CARD: blank / zeros → `Card number not provided`; not numeric → `CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER` | no edit | `@CardNumber` path constraint with both messages (400) |
| 9100-GETCARD-BYACCTCARD reads CARDDAT **by card number only**; NOTFND → `Did not find cards for this search condition` | 404 `Card … not found` | 404 with the COBOL message |
| 2210-EDIT-ACCOUNT requires an account number but the read never uses it | – | not required. Hypothesis: the account field is a search prompt only; requiring it would add a check the COBOL does not make |
| Screen shows name, expiry month/year, status; no CVV | same | same, plus `version` for the update |

## COCRDUPC (CCUP) → `PUT /api/v1/cards/{cardNumber}`

| COBOL (fact) | Before | Now |
|---|---|---|
| Editable fields: `CRDNAME`, `CRDSTCD`, `EXPMON`, `EXPYEAR` (COCRDUP.bms); `EXPDAY` is dark/protected, no CVV field | full `expirationDate`, optional `cvv` | `embossedName`, `activeStatus`, `expirationMonth`, `expirationYear`, `version`; day and CVV cannot be changed |
| 1230-EDIT-NAME: blank → `Card name not provided`; only `A-Z a-z` and spaces → `Card name can only contain alphabets and spaces` | allowed `.'-`, generic message | letters and spaces only, COBOL messages |
| 1240-EDIT-CARDSTATUS: `FLG-YES-NO-VALID VALUES 'Y','N'` → `Card Active Status must be Y or N` | accepted `y`/`n` | `Y` / `N` only |
| 1250-EDIT-EXPIRY-MON: `VALID-MONTH 1 THRU 12` → `Card expiry month must be between 1 and 12` | **missing** | `@Min(1) @Max(12)` |
| 1260-EDIT-EXPIRY-YEAR: `VALID-YEAR 1950 THRU 2099` → `Invalid card expiry year` | only year > 2099 (422) | `@Min(1950) @Max(2099)`, 400 |
| 1200-EDIT-MAP-INPUTS: `UPPER-CASE(new) = UPPER-CASE(old)` → `No change detected with respect to values fetched.`, nothing written | always rewrote | 422 with that message, no rewrite, version unchanged |
| 9200-WRITE-PROCESSING: `READ … UPDATE`, then 9300-CHECK-CHANGE-IN-REC compares with the copy shown to the user → `Record changed by some one else. Please review` | `@Version` within one request only | `PESSIMISTIC_WRITE` read (`findByCardNum`) + `version` from the client compared with the stored one → 409 with the COBOL message (conventions.md §7 hypothesis now implemented for cards) |
| REWRITE: name as typed (not upper-cased), status, `YYYY-MM-DD` from new year/month and the old day | name and status upper-cased | name stored as typed (trailing blanks trimmed), status as typed, stored day kept |
| REWRITE moves `CC-ACCT-ID-N` (the typed account) into `CARD-UPDATE-ACCT-ID` and `CCUP-NEW-CVV-CD` (never filled from the screen) into the CVV | – | **not ported**: stored account id and CVV are kept. Hypothesis: both are COBOL defects (the read is by card number only, so a different typed account would move the card; the CVV field is never set from the map) |
| Stored day not valid in the new month (e.g. 31 → February): COBOL writes the string as is | – | Hypothesis: last day of the month (a `DATE` column cannot hold `2031-02-31`) |
| Two steps: Enter validates (`Changes validated.Press F5 to save`), PF5 writes | one `PUT` | one `PUT` validates and writes; the confirmation is a client concern |

## Not verified

- Behaviour against a CICS region (none available); all rows above are covered by MockMvc tests on
  Testcontainers PostgreSQL and by a run of the service jar against the loaded sample data (PR description).
