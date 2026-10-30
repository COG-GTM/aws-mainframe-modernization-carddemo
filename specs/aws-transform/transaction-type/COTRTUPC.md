# Program overview - COTRTUPC (CICS transaction CTTU, map COTRTUP/CTRTUPA)

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

## Purpose
Maintains a single transaction type: look it up by key, then show, update, add (if not found) or delete it.

## State machine (`TTUP-CHANGE-ACTION`)
| State | Meaning | Next |
|---|---|---|
| K | invalid search key | re-prompt `Enter transaction type to be maintained` |
| X | key not found | `Press F05 to add. F12 to cancel` -> R |
| R / V | entering / reviewing a new record | F5 save |
| S | details shown | edit -> N, PF4 -> 9 |
| E | changes not OK (edit failed) | fix and Enter |
| N | changes validated, not confirmed | `Changes validated.Press F5 to save` |
| C / F / L | saved / failed / lock error | `Changes committed to database` / `Changes unsuccessful` |
| 9 -> 8 -> 7/6 | delete confirm (`Delete this record ? Press F4 to confirm`) -> start -> done/failed | |
| B | F12 backed out changes | |

## Edits
- 1210-EDIT-TRANTYPE / 1245-EDIT-NUM-REQD: the code is required (BR-TT-001), numeric via TEST-NUMVAL (BR-TT-002) and not zero (BR-TT-003). It is moved into a 2-digit numeric, so `7` becomes `07` (BR-TT-004).
- 1230-EDIT-ALPHANUM-REQD: the description is required (BR-TT-005), may only contain letters, digits and spaces (BR-TT-006) and is at most 50 characters after trimming (BR-TT-007).
- 1205-COMPARE-OLD-NEW: if the old and new values match after UPPER-CASE + TRIM, there is no change (BR-TT-008).

## Db2
- Read: `SELECT ... WHERE TR_TYPE = :key`. +100 means not found (BR-TT-020), negative means a generic error (BR-TT-060).
- 9600-WRITE-PROCESSING: UPDATE first. +100 falls back to INSERT (BR-TT-042), -911 is a lock error (BR-TT-041), other negatives are failures (BR-TT-060). INSERT failure: `Error inserting record into: TRANSACTION_TYPE Table.` (BR-TT-030).
- 9800-DELETE-PROCESSING: -532 is child rows (BR-TT-051), other errors are failures (BR-TT-060). Delete needs two PF4 presses (BR-TT-050).

## PF keys
PF3 exit (to caller or admin menu), PF4 delete / confirm delete, PF5 save, PF12 cancel / back out.

Rules: BR-TT-001..008, 020, 030, 041, 042, 050, 051, 060.
