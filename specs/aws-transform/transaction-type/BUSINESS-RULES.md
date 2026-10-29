# Business rules catalog - transaction type

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

| ID | Rule | Source |
|---|---|---|
| BR-TT-001 | Type code is required (`Tran Type code must be supplied.`) | COTRTUPC 1245-EDIT-NUM-REQD |
| BR-TT-002 | Type code must be numeric, at most 2 digits | COTRTUPC 1245 (TEST-NUMVAL) |
| BR-TT-003 | Type code must not be zero | COTRTUPC 1245 |
| BR-TT-004 | Type code is stored as two digits (left zero pad) | COTRTUPC 1210 (WS-EDIT-NUMERIC-2) |
| BR-TT-005 | Description is required | COTRTUPC 1230, COTRTLIC 1211 |
| BR-TT-006 | Description may contain only letters, digits and spaces | COTRTUPC 1230 (INSPECT CONVERTING) |
| BR-TT-007 | Description is at most 50 characters, trimmed | DDL VARCHAR(50), COTRTUPC 1230 |
| BR-TT-008 | Update with no change (case-insensitive, trimmed) is rejected | COTRTUPC 1205, COTRTLIC 1211 |
| BR-TT-010 | List is ordered by TR_TYPE, 7 rows per page | COTRTLIC WS-MAX-SCREEN-LINES |
| BR-TT-011 | Type filter: blank/zero means none, otherwise exactly 2 digits | COTRTLIC 1220-EDIT-TYPECD |
| BR-TT-012 | Description filter is a contains match `LIKE '%' || TRIM(x) || '%'` | COTRTLIC 1230-EDIT-DESC |
| BR-TT-013 | Forward page: `TR_TYPE >= start`; the 8th row decides whether there is a next page and is the next start key | COTRTLIC 8000-READ-FORWARD |
| BR-TT-014 | Backward page: `TR_TYPE < first`, read descending, shown ascending | COTRTLIC 8100-READ-BACKWARDS |
| BR-TT-015 | PF7 on page 1 / PF8 when the last page is already shown gives `No previous pages to display` / `No more pages to display` | COTRTLIC 0000-MAIN PF7/PF8 |
| BR-TT-016 | No matching rows gives `No records found for this search condition.` | COTRTLIC 9100-CHECK-FILTERS |
| BR-TT-017 | Filters combine with AND; a filter change restarts at page 1 | COTRTLIC cursor WHERE, 1200 |
| BR-TT-018 | Only one action per interaction; only U/D are valid actions | COTRTLIC 1210-EDIT-ARRAY |
| BR-TT-019 | PF8 that reaches the final page (8th-row fetch = +100) gives `No more pages for these search conditions` *(added by gap review)* | COTRTLIC 8000-READ-FORWARD WS-MESG-NO-MORE-RECORDS |
| BR-TT-020 | Lookup of a missing key gives `No record found for this key in database` | COTRTUPC 9100-GET-TRANSACTION-TYPE |
| BR-TT-030 | Add inserts a new row; insert failure (e.g. -803 duplicate) is reported with SQLCODE | COTRTUPC 9700-INSERT-RECORD |
| BR-TT-040 | Update hitting +100 gives `Record not found. Deleted by others ? ` | COTRTLIC 9200-UPDATE-RECORD |
| BR-TT-041 | SQLCODE -911 on update gives `Deadlock. Someone else updating ?` | COTRTLIC 9200, COTRTUPC 9600 |
| BR-TT-042 | Save from the maintenance screen = UPDATE, falling back to INSERT on +100 | COTRTUPC 9600-WRITE-PROCESSING |
| BR-TT-050 | Delete needs an explicit second confirmation | COTRTUPC PF4 x2, COTRTLIC D + F10 |
| BR-TT-051 | Delete blocked by child categories (-532) gives `Please delete associated child records first:` | COTRTLIC 9300, COTRTUPC 9800 |
| BR-TT-052 | Delete of a row that is already gone (+100) is reported as a generic failure `Delete failed with message:` + SQLCODE *(wording corrected by gap review)* | COTRTLIC 9300 WHEN OTHER |
| BR-TT-060 | Any other negative SQLCODE gives a generic Db2 error message carrying the SQLCODE | 9999-FORMAT-DB2-MESSAGE |
| BR-TT-061 | Db2 connectivity is checked before serving (priming query) | COTRTLIC 9998-PRIMING-QUERY |
| BR-TT-070 | Batch input record = action X(1) + code X(2) + description X(50) | COBTUPDT FD |
| BR-TT-071 | Batch actions: A insert, U update, D delete, * comment | COBTUPDT 1003-TREAT-RECORD |
| BR-TT-072 | Unknown batch action gives `ERROR: TYPE NOT VALID`, rc 4 | COBTUPDT WHEN OTHER |
| BR-TT-073 | Batch U/D on a missing key (+100) gives `No records found.`, rc 4 | COBTUPDT 10032/10033 |
| BR-TT-074 | Batch negative SQLCODE gives an error with SQLCODE, rc 4 | COBTUPDT 1003x |
| BR-TT-075 | A batch error does not stop the run; rc 4 stays set | COBTUPDT 9999-ABEND |
| BR-TT-076 | Batch DISPLAYs each record and the outcome to SYSOUT | COBTUPDT DISPLAYs |
| BR-TT-080 | Type extract = 60-byte records `code + CHAR(50) desc + 8 zeros`, by code | TRANEXTR STEP40 |
| BR-TT-081 | Category extract = 60-byte records `code + cat + CHAR(50) desc + 4 zeros` | TRANEXTR STEP50 |
