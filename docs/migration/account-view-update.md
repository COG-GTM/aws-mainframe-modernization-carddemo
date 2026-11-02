# Account view and update: COACTVWC / COACTUPC against account-service

Ticket UNT149-17. The COBOL under `app/` is the reference and was only read. **Hypothesis** marks inferences.

## 1. Endpoints

| CICS | Java | Notes |
|---|---|---|
| CAVW `COACTVWC` | `GET /api/v1/accounts/{accountId}/view` → `AccountViewResponse` | COACTVW output fields plus `accountVersion` / `customerVersion` |
| CAUP `COACTUPC` (PF5 path) | `PUT /api/v1/accounts/{accountId}` with `AccountUpdateRequest` → `AccountViewResponse` | the unprotected COACTUP input fields plus both versions |
| — | `GET /api/v1/accounts/{accountId}` | unchanged: the ACCTDAT record alone, used by transaction-service and the batch jobs |

Data path (COBOL `9000-READ-ACCT`): account id → CXACAIX (`GET /api/v1/card-xrefs/by-account/{id}` on card-service) →
ACCTDAT (account-service) → CUSTDAT (`GET /api/v1/customers/{custId}` on customer-service). Both clients forward the
caller's bearer token (`CallerTokenForwarder.forCallers()`).

## 2. Paragraph review

| COBOL | Behaviour (from source) | Java |
|---|---|---|
| COACTVWC `2210-EDIT-ACCOUNT` | blank → `Account number not provided`; not numeric or zero → `Account Filter must  be a non-zero 11 digit number` | path variable can't be blank; 1–11 digits, non-zero, else 400 with that text (`AccountViewService.parseAccountId`) |
| COACTUPC `1210-EDIT-ACCOUNT` | same check, message `Account Number if supplied must be a 11 digit Non-Zero Number` | same, 400 |
| `9200-GETCARDXREF-BYACCT` NOTFND | `Account:<id> not found in Cross ref file.  Resp:<resp> Reas:<resp2>` | 404 `Account:<11 digits> not found in Cross ref file.` (CICS RESP/RESP2 dropped) |
| `9300-GETACCTDATA-BYACCT` NOTFND | `Account:<id> not found in Acct Master file.Resp:...` | 404 `Account:<id> not found in Acct Master file.` |
| `9400-GETCUSTDATA-BYCUST` NOTFND | `CustId:<id> not found in customer master.Resp: ...` | 404 `CustId:<9 digits> not found in customer master.` |
| COACTVWC `1200-SETUP-SCREEN-VARS` | account + customer fields, SSN shown `999-99-9999` | `AccountViewResponse`; SSN `***-**-9999` (§4) |
| COACTUPC `1100-RECEIVE-MAP` | `*` or blank input → LOW-VALUES; amounts through `NUMVAL-C` | `CobolEdits.normalize`, `CobolEdits.numvalC`; request fields are strings (JSON numbers accepted) |
| `1205-COMPARE-OLD-NEW` | runs before the edits; no difference → `No change detected with respect to values fetched.`; text compared upper-cased/trimmed | `AccountChangeDetector`, 422 `BusinessRuleException` |
| `1200-EDIT-MAP-INPUTS` | 25 edits in fixed order, first failing message kept | `AccountUpdateValidator` (§3); 400, `message` = first COBOL message, `fieldErrors` = one entry per failing field in COBOL order |
| `9600-WRITE-PROCESSING` | READ UPDATE ACCTDAT and CUSTDAT, `9700-CHECK-CHANGE-IN-REC`, REWRITE account, REWRITE customer, `SYNCPOINT ROLLBACK` if the customer rewrite fails | request versions vs stored `@Version`s → 409 `Record changed by some one else. Please review`; `saveAndFlush` account (JPA version check), then customer-service PUT with the customer version; any failure there rolls the account back |
| `9600` lock failure (`Could not lock ... for update`) / `Update of record failed` | `Changes unsuccessful. Please try again` | optimistic locking has no lock step; a concurrent writer surfaces as 409, other failures as 500 |
| `3000-SEND-MAP` / PF keys / info messages | screen state machine | not ported: the PUT is the PF5 confirmation (api-style §1), success is the 2xx |

## 3. Edits (`AccountUpdateValidator.RULES`)

Order and labels as in `1200-EDIT-MAP-INPUTS`; every message is in `RULES[].messages` (enforced by
`AccountUpdateValidatorTest`). Lookup tables are `UsLookupCodes` (generated from `CSLKPCDY`, checked against the copybook by `UsLookupCodesTest`).

| # | Field | Label | Edit |
|---|---|---|---|
| 1 | activeStatus | Account Status | `1220-EDIT-YESNO`: blank or `0` → must be supplied; only upper-case `Y`/`N` |
| 2, 4, 6 | openDate, expirationDate, reissueDate | Open Date, Expiry Date, Reissue Date | CSUTLDPY `EDIT-DATE-CCYYMMDD`: 4-digit year 19xx/20xx, month 1–12, day 1–31, 31-day months, Feb 30, leap year |
| 3, 5, 7–9 | creditLimit, cashCreditLimit, currentBalance, currentCycleCredit, currentCycleDebit | Credit Limit, Cash Credit Limit, Current Balance, Current Cycle Credit Limit, Current Cycle Debit Limit | `1250-EDIT-SIGNED-9V2`: `TEST-NUMVAL-C` (sign, `$`, commas, CR/DB); negative values pass |
| 10 | ssn | SSN | `1265-EDIT-US-SSN`: 3/2/4 digits, none zero, first part not 000/666/900–999 |
| 11 | dateOfBirth | Date of Birth | date edit + `EDIT-DATE-OF-BIRTH` (must be before today) |
| 12 | ficoScore | FICO Score | 3 digits, non-zero, 300–850 |
| 13–15 | firstName, middleName, lastName | First/Middle/Last Name | letters and spaces; middle optional |
| 16 | addressLine1 | Address Line 1 | mandatory |
| 17 | stateCode | State | letters, then `VALID-US-STATE-CODE` |
| 18 | zip | Zip | 5 digits, non-zero |
| 19 | city | City | letters and spaces, required |
| 20 | countryCode | Country | letters, required — run on the stored value (field protected on the map) |
| 21–22 | phoneNumber1, phoneNumber2 | Phone Number 1/2 | `1260-EDIT-US-PHONE-NUM`: optional; area code in `VALID-GENERAL-PURP-CODE`, 3/3/4 digits, none zero |
| 23 | eftAccountId | EFT Account Id | 10 digits, non-zero |
| 24 | primaryCardHolder | Primary Card Holder | Y/N |
| 25 | zip | — | `1280-EDIT-US-STATE-ZIP-CD`: state + first two ZIP digits in `VALID-US-STATE-ZIP-CD2-COMBO`, only when 17 and 18 passed |

Not edited by the COBOL, so not edited here: group id, address line 2, government issued id.

## 4. Differences found and what was done

Fixed (Java now follows the COBOL):

- The old `PUT` updated account fields only; COACTUPC also rewrites CUSTDAT. The PUT now carries the customer fields and calls customer-service.
- The old `PUT` had three rules the COBOL does not have (expiry ≥ open, reissue ≥ open, cash limit ≤ credit limit) and rejected negative limits; removed. It did not allow editing the current balance and cycle credit/debit, which COACTUPC does (unprotected in `3320-UNPROTECT-FEW-ATTRS`); added.
- Status accepted `y`/`n` and upper-cased them; COBOL `FLG-YES-NO-ISVALID` is `'Y'`/`'N'` only.
- No field edits, no lookup tables, no no-change check, no version check on the customer; all added.
- The view returned the account record only; it now follows xref → account → customer.

Kept as deliberate deviations:

- **SSN masked** in the view (customer-service decision, `API-CONTRACT.md`). So `ssn` is optional on the PUT: omitted or blank keeps the stored SSN and skips `1265-EDIT-US-SSN`; a supplied SSN is edited and always counts as a change for the no-change check (the stored SSN isn't visible to account-service).
- **Edit order vs conflict.** With matching versions the order is COBOL's (no-change, edits, conflict). With a stale version the no-change check is skipped (the COMMAREA copy it compares against isn't available) and the edits still run before the 409.
- **Amount overflow.** `COMPUTE ... = NUMVAL-C(...)` into `S9(10)V99` silently drops high-order digits; Java rejects more than 10 integer digits with `<label> is not valid`. Extra decimals are truncated, as in the COBOL.
- **Blank phone** is stored as null; the COBOL `STRING`s LOW-VALUES between the parentheses.
- **CICS RESP/RESP2** suffixes are dropped from not-found messages.
- Month/day accept one or two digits (`2020-1-5`); the map fields are `JUSTIFY=(RIGHT)` and **Hypothesis:** a right-justified `' 1'` passes `TEST-NUMVAL` and `WS-VALID-MONTH`.

COBOL defects found (reported, not reproduced unless noted):

- `ACCT-UPDATE-RECORD` has no `ACCT-ADDR-ZIP`, so `ACCT-UPDATE-GROUP-ID` sits at offset 102, where `CVACT01Y` keeps `ACCT-ADDR-ZIP`; the REWRITE writes the group id into the zip bytes and blanks `ACCT-GROUP-ID`. Java writes the group id to `group_id` and leaves `addr_zip` alone.
- `1260-EDIT-US-PHONE-NUM`'s all-blank test checks the area code twice and never the line number (`WS-EDIT-US-PHONE-NUMA EQUAL SPACES OR WS-EDIT-US-PHONE-NUMC EQUAL LOW-VALUES`), so a phone with blank area code and prefix passes whatever the line number is. **Reproduced** (`CobolEdits.usPhone`), because it changes which inputs are accepted.
- `CSUTLDPY`'s final `CALL 'CSUTLDTC'` can't fail for a date that passed the earlier edits with a 19xx/20xx year; it has no Java counterpart.

Known limitation: account and customer live in two services, so the two rewrites are not one unit of work. A failure in the customer PUT rolls back the account; a failure committing the account after the customer PUT succeeded would leave the customer updated (same class of problem as the posting limitation in architecture.md §1).
