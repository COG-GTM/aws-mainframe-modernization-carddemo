# Account view / update (CAVW / CAUP)

Programs: `COACTVWC` (COACTVW), `COACTUPC` (COACTUP). Files: ACCTDAT,
CUSTDAT, CARDXREF (via account alternate index CXACAIX).

| ID | Steps | Expected |
|---|---|---|
| ACV-01 | CAVW; Account = an existing 11-digit account from acctdata; ENTER | Account fields (status, balances, limits, dates, group) and customer fields displayed |
| ACV-02 | Account blank; ENTER | "Account number not provided" style error |
| ACV-03 | Account = `ABC`, or all zeros | Must be a non-zero 11-digit number error |
| ACV-04 | Account = valid format, not on file | Account not found error |
| ACV-05 | PF3 | Return to calling menu |
| ACU-01 | CAUP; fetch existing account; change Credit Limit; ENTER, then PF5 | Confirmation prompt, then "Changes committed" style message; ACCTDAT shows new limit |
| ACU-02 | Change Active Status to `X` | Validation error (must be Y/N); no update |
| ACU-03 | Open date = 2023-02-30 | Date validation error (via CSUTLDTC / date edits); no update |
| ACU-04 | Customer phone/SSN/ZIP/state invalid (e.g. state `ZZ`) | Field-specific error, cursor on field (rules in `CSLKPCDY.cpy`) |
| ACU-05 | Make changes then PF12 | Changes discarded, original data redisplayed |
| ACU-06 | Two terminals update same account; second saves after first | Second gets "record changed by someone else" style message, no overwrite |
