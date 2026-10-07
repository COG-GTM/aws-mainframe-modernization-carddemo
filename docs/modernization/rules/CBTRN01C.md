# CBTRN01C — Daily transaction card/account check (job POSTTRAN, Java STEP10)

Source: `app/cbl/CBTRN01C.cbl`. Batch, no CICS. Files: `DALYTRAN` sequential input (`CVTRA06Y`, 350 bytes);
`CUSTFILE` (`CVCUS01Y`), `XREFFILE` (`CVACT03Y`, key `XREF-CARD-NUM X(16)`), `CARDFILE` (`CVACT02Y`), `ACCTFILE`
(`CVACT01Y`, key `ACCT-ID 9(11)`), `TRANFILE` (`CVTRA05Y`) — all KSDS opened `INPUT`. It changes no data: it
DISPLAYs every daily transaction and whether its card and account resolve. `app/jcl/POSTTRAN.jcl` runs only CBTRN02C;
the Java `posttran` stream runs this program first as `STEP10` (ticket s4.2), and CBTRN02C (`STEP15`) has
`COND=(4,LT,STEP10)`, so posting is bypassed only if this check abends or ends above RC 4.
Java: `com.carddemo.batch.posttran.Cbtrn01c`, job `cbtrn01c`, wired in `PosttranJobConfiguration`.

| # | Given | Then |
|---|---|---|
| R-1 | Start | `START OF EXECUTION OF PROGRAM CBTRN01C`; open the six files in DD order DALYTRAN, CUSTFILE, XREFFILE, CARDFILE, ACCTFILE, TRANFILE. |
| R-2 | An open ends with a status other than `00` | `ERROR OPENING <file>`, `FILE STATUS IS: NNNN<status>`, `ABENDING PROGRAM`, abend U999 (Java RC 16). |
| R-3 | `READ DALYTRAN` status `00` | DISPLAY the whole 350-byte `DALYTRAN-RECORD`. |
| R-4 | Status `10` | End of file; the loop body still performs R-5..R-7 once more with the last record (the `IF END-OF-DAILY-TRANS-FILE = 'N'` around the lookup is tested before the READ), so the last card is looked up twice. |
| R-5 | Any other READ status | `ERROR READING DAILY TRANSACTION FILE` + status + abend. |
| R-6 | `XREF-CARD-NUM ← DALYTRAN-CARD-NUM`; `READ XREFFILE` INVALID KEY | `INVALID CARD NUMBER FOR XREF`, then `CARD NUMBER <card> COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-<id>`. |
| R-7 | Card found | `SUCCESSFUL READ OF XREF`, `CARD NUMBER: `, `ACCOUNT ID : `, `CUSTOMER ID: `; then `ACCT-ID ← XREF-ACCT-ID`, `READ ACCTFILE`: INVALID KEY → `INVALID ACCOUNT NUMBER FOUND` + `ACCOUNT <id> NOT FOUND`; found → `SUCCESSFUL READ OF ACCOUNT FILE`. |
| R-8 | End | Close the six files (a non-`00` close abends as R-2 with `ERROR CLOSING <file>`); `END OF EXECUTION OF PROGRAM CBTRN01C`; RC 0. |

Baseline (`docs/validation/baseline/CBTRN01C`): 300 records read, RC 0; SYSOUT reproduced line for line in file and
table mode (`PosttranBaselineTest`, `PosttranJobIT`, `scripts/batch/run_posttran.sh`).
