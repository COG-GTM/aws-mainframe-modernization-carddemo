# Program overview - COTRTLIC (CICS transaction CTLI, map COTRTLI/CTRTLIA)

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

## Purpose
Admin list of transaction types from `CARDDEMO.TRANSACTION_TYPE`. Shows 7 rows per screen, with optional filters. A row can be selected with `U` (inline description update) or `D` (delete), then confirmed with F10.

## Inputs / outputs
- Input: BMS map fields `TRTYPE` (type filter), `TRDESC` (description filter), 7 selection fields + 7 editable description fields; COMMAREA carries paging keys, the selected row and the pending action.
- Output: map with rows, page number, info / error messages; Db2 updates and deletes; XCTL to `COADM01C` (PF3) or `COTRTUPC` (PF2 add).

## Flow
1. 9998-PRIMING-QUERY: `SELECT 1 FROM SYSIBM.SYSDUMMY1`. If Db2 can't be reached, the program shows the error instead of the list (BR-TT-061).
2. Edit filters (BR-TT-011, BR-TT-012). Changing a filter resets paging to the first page (BR-TT-017).
3. Edit selections. Only one action is allowed at a time (`Please select only 1 action`). Anything other than U, D or blank gives `Action code selected is invalid` (BR-TT-018).
4. Read forward with cursor `C-TR-TYPE-FORWARD` (`TR_TYPE >= :start`, filters, ORDER BY TR_TYPE). It fills 7 rows and fetches an 8th to decide whether another page exists. The 8th key becomes the next start key (BR-TT-010, BR-TT-013).
5. PF7 uses cursor `C-TR-TYPE-BACKWARD` (`TR_TYPE < :first`, ORDER BY TR_TYPE DESC) (BR-TT-014). The first-page and last-page boundaries show messages (BR-TT-015). A PF8 that lands on the final page shows `No more pages for these search conditions` (BR-TT-019, added by gap review). If nothing matches: `No records found for this search condition.` (BR-TT-016).
6. `U` + F10: the description is edited (BR-TT-005..008), then UPDATE runs. +100 gives `Record not found. Deleted by others ? ` (BR-TT-040), -911 gives `Deadlock. Someone else updating ?` (BR-TT-041), any other negative gives `Update failed with` + SQLCODE (BR-TT-060). Success ends with SYNCPOINT.
7. `D` highlights the row with `Delete HIGHLIGHTED row ? Press F10 to confirm`. F10 runs the DELETE (BR-TT-050). -532 gives `Please delete associated child records first:` (BR-TT-051). Any other non-zero SQLCODE gives `Delete failed with message:` + SQLCODE (BR-TT-052, BR-TT-060).

## PF keys
| Key | Action |
|---|---|
| Enter | apply filters / selections |
| PF2 | XCTL to COTRTUPC (add) |
| PF3 | return to admin menu COADM01C |
| PF7 / PF8 | page backward / forward |
| PF10 | confirm the pending U or D |

Rules: BR-TT-005..008, 010..019, 040, 041, 050..052, 060, 061.
