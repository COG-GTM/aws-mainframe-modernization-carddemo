# SME approval checklist: CBACT04C and CBTRN02C business rules

One row per rule in [CBACT04C.md](CBACT04C.md) and [CBTRN02C.md](CBTRN02C.md). Read the rule and its cited source,
then tick **Approve** (the rule states what the program does and the port should keep it), **Change** (the port
should behave differently; write the change in Comments) or **Question** (needs discussion before a decision). Add
your initials. The two port PRs cite these rule ids and do not merge until every row has Approve or Change.

Rules marked SME question in the catalog (INT-05, INT-18, POST-11, POST-16) are the ones most likely to need
Change.

| Rule | Class | Summary | Approve | Change | Question | Initials | Comments |
|------|-------|---------|---------|--------|----------|----------|----------|
| INT-01 | control flow | PARM-DATE (10 bytes) is the only run parameter and feeds TRAN-ID only | [ ] | [ ] | [ ] | | |
| INT-02 | file I/O | Open modes of the five files; all closed at end | [ ] | [ ] | [ ] | | |
| INT-03 | control flow | Sequential TCATBALF driver loop in key order until status 10 | [ ] | [ ] | [ ] | | |
| INT-04 | control flow | Account break: update previous account, reset total, read account and xref | [ ] | [ ] | [ ] | | |
| INT-05 | control flow | SME question: last account is never REWRITTEN (unreachable ELSE) | [ ] | [ ] | [ ] | | |
| INT-06 | validation | Account read by TRANCAT-ACCT-ID; missing account abends | [ ] | [ ] | [ ] | | |
| INT-07 | validation | Xref read by alternate key XREF-ACCT-ID for the card number; miss abends | [ ] | [ ] | [ ] | | |
| INT-08 | calculation | Rate key = ACCT-GROUP-ID + type code + category code | [ ] | [ ] | [ ] | | |
| INT-09 | validation | Status 23 retries with group DEFAULT; DEFAULT miss abends | [ ] | [ ] | [ ] | | |
| INT-10 | control flow | Zero rate: no interest, no transaction, no fee call | [ ] | [ ] | [ ] | | |
| INT-11 | calculation | Interest = bal x rate / 1200, truncated to cents (no ROUNDED) | [ ] | [ ] | [ ] | | |
| INT-12 | calculation | Interest accumulated per account in WS-TOTAL-INT | [ ] | [ ] | [ ] | | |
| INT-13 | file I/O | One 350-byte interest transaction written per non-zero-rate category | [ ] | [ ] | [ ] | | |
| INT-14 | calculation | TRAN-ID = PARM-DATE + 6-digit run-wide counter | [ ] | [ ] | [ ] | | |
| INT-15 | calculation | Fixed fields: type 01, category 05, source System, description, card number | [ ] | [ ] | [ ] | | |
| INT-16 | calculation | ORIG-TS and PROC-TS from FUNCTION CURRENT-DATE | [ ] | [ ] | [ ] | | |
| INT-17 | file I/O | Account REWRITE: balance += total interest, cycle credit/debit set to 0 | [ ] | [ ] | [ ] | | |
| INT-18 | calculation | SME question: 1400-COMPUTE-FEES is a stub; no fees | [ ] | [ ] | [ ] | | |
| INT-19 | error handling | Any bad file status: DISPLAY, then CEE3ABD abend 999 | [ ] | [ ] | [ ] | | |
| INT-20 | control flow | Normal end returns RC 0; no reject path | [ ] | [ ] | [ ] | | |
| POST-01 | file I/O | Open modes of the six files; all closed at end | [ ] | [ ] | [ ] | | |
| POST-02 | control flow | DALYTRAN driver loop: count, reset reason, validate, post or reject | [ ] | [ ] | [ ] | | |
| POST-03 | validation | Order: xref, then account; no other validations | [ ] | [ ] | [ ] | | |
| POST-04 | validation | 100: card number not in XREFFILE | [ ] | [ ] | [ ] | | |
| POST-05 | validation | 101: account not in ACCTFILE | [ ] | [ ] | [ ] | | |
| POST-06 | validation | 102: cycle credit - cycle debit + amount > credit limit | [ ] | [ ] | [ ] | | |
| POST-07 | validation | 103: received after expiry; overwrites 102 | [ ] | [ ] | [ ] | | |
| POST-08 | file I/O | Reject record: 350-byte input + 80-byte trailer (LRECL 430) | [ ] | [ ] | [ ] | | |
| POST-09 | control flow | RC 4 when any record is rejected, else RC 0 | [ ] | [ ] | [ ] | | |
| POST-10 | calculation | Posted record copies DALYTRAN; PROC-TS replaced | [ ] | [ ] | [ ] | | |
| POST-11 | control flow | SME question: TCATBAL, account, transaction order with no rollback | [ ] | [ ] | [ ] | | |
| POST-12 | file I/O | TCATBALF key; status 23 creates, 00 updates, other abends | [ ] | [ ] | [ ] | | |
| POST-13 | calculation | Create: initialize, balance = amount, WRITE | [ ] | [ ] | [ ] | | |
| POST-14 | calculation | Update: balance += amount, REWRITE | [ ] | [ ] | [ ] | | |
| POST-15 | calculation | Account balance += amount; credit if >= 0 else debit | [ ] | [ ] | [ ] | | |
| POST-16 | error handling | SME question: 109 on REWRITE failure is set but never acted on | [ ] | [ ] | [ ] | | |
| POST-17 | file I/O | Transaction WRITE by TRAN-ID; duplicate or other error abends | [ ] | [ ] | [ ] | | |
| POST-18 | calculation | PROC-TS from FUNCTION CURRENT-DATE | [ ] | [ ] | [ ] | | |
| POST-19 | error handling | Any bad file status: DISPLAY, then CEE3ABD abend 999 | [ ] | [ ] | [ ] | | |

Reviewer name: ______________________  Date: ____________

Rows: 20 for CBACT04C and 19 for CBTRN02C.
