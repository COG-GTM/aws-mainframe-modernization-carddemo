# Gap review - legacy COBOL vs spec vs Java

> **Provenance:** Authored by **Devin** in the AWS Transform for mainframe *business logic extraction* layout (program overview, business rules, data dictionary, flows). **Not produced by AWS Transform.** The real AWS Transform job (workspace `CardDemo-Transform-Demo`, job `CardDemo-Transaction-Type-Db2`) was started on the same source; see `README.md` for its status.

How this was done: each paragraph of `COTRTLIC.cbl`, `COTRTUPC.cbl`, `COBTUPDT.cbl` and `TRANEXTR.jcl` was read next to the spec files and `modernization/transaction-type-service`. Every string literal was grepped out of the COBOL and compared with the Java messages. The Java column was checked with `mvn -Pdb2-it verify` (unit + MockMvc + real Db2 integration tests).

Legend: Spec = captured in the first spec draft (**MISSED** = absent, added by this review). Java = implemented + tested (**FIXED** = code changed by this review).

## SQLCODE handling
| Legacy behaviour | Where | Spec | Java |
|---|---|---|---|
| 0: SYNCPOINT after update/delete | COTRTLIC 9200/9300 | yes | yes (auto-commit per statement) |
| +100 on single read: `No record found for this key in database` | COTRTUPC 9100 | BR-TT-020 | yes |
| +100 on update: `Record not found. Deleted by others ? ` | COTRTLIC 9200 | BR-TT-040 | yes |
| +100 on save = INSERT instead | COTRTUPC 9600 | BR-TT-042 | yes |
| +100 on delete is reported as a failure through **WHEN OTHER** (`Delete failed with message:` + SQLCODE), not as "deleted by others" | COTRTLIC 9300 | BR-TT-052 (wording imprecise) | **FIXED**: the message was `Record not found. Deleted by others ?`, now `Delete failed with message: SQLCODE:+100` (HTTP 404) |
| -803 duplicate on insert: `Error inserting record into: TRANSACTION_TYPE Table.` | COTRTUPC 9700 | BR-TT-030 | yes (409; the -803 comes from the real Db2 IT) |
| -911 deadlock / timeout on update: `Deadlock. Someone else updating ?` | COTRTLIC 9200, COTRTUPC 9600 | BR-TT-041 | yes (409) |
| -911 on **delete** gets no special case (falls to WHEN OTHER) | COTRTLIC 9300 | implied by BR-TT-060 | yes (generic 500 carrying the SQLCODE) |
| -532 FK restrict: `Please delete associated child records first:` | COTRTLIC 9300, COTRTUPC 9800 | BR-TT-051 | yes (409; the -532 comes from the real Db2 IT) |
| other negative codes: 9999-FORMAT-DB2-MESSAGE with SQLCODE | all | BR-TT-060 | yes |
| priming query `SELECT 1 FROM SYSIBM.SYSDUMMY1` before any work | COTRTLIC 9998 | BR-TT-061 | yes (503 + actuator health); **test added** in this review |

## Paging / PF keys (COTRTLIC)
| Legacy behaviour | Spec | Java |
|---|---|---|
| 7 rows per screen ordered by TR_TYPE | BR-TT-010 | yes |
| forward cursor `>= start`, 8th-row peek sets next key | BR-TT-013 | yes |
| backward cursor `< first` DESC, shown ascending | BR-TT-014 | yes |
| PF7 on page 1: `No previous pages to display` | BR-TT-015 | yes |
| PF8 when the last page is already shown: `No more pages to display` | BR-TT-015 | yes |
| PF8 that **lands on** the last page (8th-row fetch returns +100): `No more pages for these search conditions` (WS-MESG-NO-MORE-RECORDS) | **MISSED** -> BR-TT-019 | **FIXED** (the Java returned `No more pages to display`); test `br_tt_019_pf8OntoLastPage` |
| no rows on screen 1: `No records found for this search condition.` | BR-TT-016 | yes |
| filter change restarts at page 1 **and discards pending row selections** (1210-EDIT-ARRAY exits early) | partial (BR-TT-017 covered the reset, not the discard) | not applicable: REST calls carry no pending selection state |
| PF2 add (XCTL COTRTUPC), PF3 exit (XCTL COADM01C) | yes (PF key table) | not applicable: navigation; add = `POST` |
| PF10 confirms the pending U/D | BR-TT-050 | yes (`confirm=true`) |

## Validation
| Legacy behaviour | Spec | Java |
|---|---|---|
| type code required / numeric / not zero (`must be supplied.`, `must be numeric.`, `must not be zero.`) | BR-TT-001..003 | yes |
| type code zero-padded to 2 digits | BR-TT-004 | yes |
| description required, letters/digits/spaces only (`can have numbers or alphabets only.`), max 50 | BR-TT-005..007 | yes |
| no-change: UPPER-CASE + TRIM compare (and equal trimmed length) | BR-TT-008 | yes |
| type filter: blank / spaces / zeros = no filter, else must be numeric (`TYPE CODE FILTER,IF SUPPLIED MUST BE A 2 DIGIT NUMBER`) | BR-TT-011 | yes |
| description filter `LIKE %TRIM(x)%`, case-sensitive | BR-TT-012 | yes (case-sensitive, matching Db2 LIKE) |
| one action per screen (`Please select only 1 action`); only U/D (`Action code selected is invalid`) | BR-TT-018 | yes by construction (one row action per HTTP call); **test added** |

## Delete confirmation
| Legacy behaviour | Spec | Java |
|---|---|---|
| COTRTLIC: `D` highlights the row, F10 deletes | BR-TT-050 | yes (428 without confirm) |
| COTRTUPC: PF4 -> `Delete this record ? Press F4 to confirm` -> PF4 | BR-TT-050 | yes (same 428 prompt text) |
| success message `Delete successful.` | yes | yes |

## Batch (COBTUPDT)
| Legacy behaviour | Spec | Java |
|---|---|---|
| 53-byte record 1+2+50 | BR-TT-070 | yes |
| A / U / D / * | BR-TT-071 | yes |
| other action: `ERROR: TYPE NOT VALID`, rc 4 | BR-TT-072 | yes |
| U/D on +100: `No records found.`, rc 4 | BR-TT-073 | yes |
| negative SQLCODE: `Error accessing: TRANSACTION_TYPE table. SQLCODE:` | BR-TT-074 | yes |
| 9999-ABEND does not stop the loop; rc 4 is kept | BR-TT-075 | yes (Db2 IT proves later records still land) |
| SYSOUT trace per record | BR-TT-076 | yes |
| no field validation in batch | yes (COBTUPDT notes) | yes (batch bypasses the online edits) |
| description stored with trailing blanks (X(50) host var) | yes (COBTUPDT notes) | **intentional deviation**: Java trims trailing blanks, so online and batch rows compare equal. Flagged for the customer to decide. |

## Extract (TRANEXTR)
| Legacy behaviour | Spec | Java |
|---|---|---|
| 60-byte type records with 8 zero fill | BR-TT-080 | yes (Db2 IT) |
| 60-byte category records with 4 zero fill | BR-TT-081 | yes (Db2 IT) |

## Summary
- **The spec missed:** (1) the third paging message, `No more pages for these search conditions`, which shows when PF8 lands on the final page (now BR-TT-019); (2) that legacy treats delete +100 as a generic failure (BR-TT-052 wording corrected); (3) that a filter change also discards pending row selections (UI-only, not applicable to REST).
- **The Java missed / changed:** the BR-TT-019 message and the BR-TT-052 message (both fixed, with tests). Tests were added for BR-TT-018 and BR-TT-061, which had code but no dedicated test.
- **Deliberate deviation:** batch descriptions are trimmed (see the batch table).

## Cross-check against the real AWS Transform technical documentation

The real AWS Transform technical documentation arrived later and is in `aws-transform-output/technical-documentation/`. A text search of its JSON for the COTRTLIC paging messages found:

| Legacy literal (COTRTLIC) | Devin spec | AWS Transform tech docs |
|---|---|---|
| `No previous pages to display` | BR-TT-014 | present |
| `No more pages to display` | BR-TT-015 | present |
| `No more pages for these search conditions` | BR-TT-019 (added in this review) | **absent**, so the generated docs miss it too |

The extra message only shows up when you read 2000-SEND-SCREEN / 9000-READ-FORWARD side by side, which is why this gap review exists.
