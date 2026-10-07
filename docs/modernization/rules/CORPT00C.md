# CORPT00C — Transaction report request (transaction CR00, map CORPT0A / mapset CORPT00)

Source: `app/cbl/CORPT00C.cbl`. No VSAM access. Output: `EXEC CICS WRITEQ TD QUEUE('JOBS')` — one 80-byte JCL card per
record, i.e. the program submits batch job `TRNRPT00` (which runs `CBTRN03C` with a `DATEPARM`-style window) to the
internal reader via an extrapartition TDQ. Calls `CSUTLDTC` (`YYYY-MM-DD`). Fields: `MONTHLY`, `YEARLY`, `CUSTOM` (1 each),
`SDTMM`/`SDTDD`(2) `SDTYYYY`(4), `EDTMM`/`EDTDD`/`EDTYYYY`, `CONFIRM`(1).

**Important:** `SEND-TRNRPT-SCREEN` ends with `GO TO RETURN-TO-CICS` (`RETURN TRANSID('CR00')`), so the first message sent
terminates the task; subsequent validations never run in that task.

## MAIN-PARA

| # | Given | Then |
|---|---|---|
| R-1 | `EIBCALEN = 0` | `CDEMO-TO-PROGRAM='COSGN00C'`; `RETURN-TO-PREV-SCREEN`. |
| R-2 | First entry | Set re-enter; clear map; send (no processing). |
| R-3 | Re-entry + `DFHENTER` | `PROCESS-ENTER-KEY`. |
| R-4 | Re-entry + `DFHPF3` | `CDEMO-TO-PROGRAM='COMEN01C'`; `RETURN-TO-PREV-SCREEN` (always the main menu, not the caller). |
| R-5 | Other AID (incl. PF7/PF8) | `Invalid key pressed. Please see below...`; send. |

## PROCESS-ENTER-KEY — report type (first non-blank selector wins: Monthly, then Yearly, then Custom)

| # | Given | Then |
|---|---|---|
| R-6 | `MONTHLYI` non-blank | `WS-REPORT-NAME='Monthly'`; start = `YYYY-MM-01` of `FUNCTION CURRENT-DATE`; end = last day of the current month, computed as `DATE-OF-INTEGER(INTEGER-OF-DATE(first day of next month) - 1)` (year rolls over when month > 12). Both dates copied to `PARM-START-DATE-1/2` and `PARM-END-DATE-1/2` in the embedded JCL; `SUBMIT-JOB-TO-INTRDR`. |
| R-7 | `YEARLYI` non-blank | `WS-REPORT-NAME='Yearly'`; start = `YYYY-01-01`, end = `YYYY-12-31` of the current year; submit. |
| R-8 | `CUSTOMI` non-blank — presence checks in order | `Start Date - Month can NOT be empty...`, `Start Date - Day can NOT be empty...`, `Start Date - Year can NOT be empty...`, `End Date - Month can NOT be empty...`, `End Date - Day can NOT be empty...`, `End Date - Year can NOT be empty...`. |
| R-9 | All present | Each of the six fields is normalised through `NUMVAL-C` into `PIC 99` / `PIC 9999` and echoed back zero-padded (`7`→`07`, non-numeric text → `00`). |
| R-10 | Range checks (string compare after normalisation) | `SDTMMI` not numeric or `> '12'` → `Start Date - Not a valid Month...`; `SDTDDI` not numeric or `> '31'` → `Start Date - Not a valid Day...`; `SDTYYYYI` not numeric → `Start Date - Not a valid Year...`; likewise `End Date - Not a valid Month...` / `End Date - Not a valid Day...` / `End Date - Not a valid Year...`. (Month `00`/day `00` pass this check.) |
| R-11 | Calendar check | `WS-START-DATE = YYYY-MM-DD` assembled and passed to `CSUTLDTC`; rejected unless `SEV-CD='0000'` or `MSG-NUM='2513'`: `Start Date - Not a valid date...`; same for end: `End Date - Not a valid date...`. No check that start ≤ end. |
| R-12 | Custom OK | Dates copied into the JCL parms; `WS-REPORT-NAME='Custom'`; `SUBMIT-JOB-TO-INTRDR`. |
| R-13 | No selector set | `Select a report type to print report...`; send. |
| R-14 | Submission completed without error | All fields initialised, `ERRMSGC=DFHGREEN`, message `<Monthly|Yearly|Custom> report submitted for printing ...`; send. |

## SUBMIT-JOB-TO-INTRDR

| # | Given | Then |
|---|---|---|
| R-15 | `CONFIRMI` blank | Message `Please confirm to print the <Monthly|Yearly|Custom> report...`; `ERR-FLG='Y'`; send (task ends; selections preserved on screen). |
| R-16 | `CONFIRMI` = `Y`/`y` | Continue to R-18. |
| R-17 | `N`/`n` | All fields initialised, `ERR-FLG='Y'`, screen re-sent with no message. Other value → message `"<value>" is not a valid value to confirm...` (value delimited by space, e.g. `"X" is not a valid value to confirm...`). |
| R-18 | Confirmed | `JOB-LINES(1..1000)` (the embedded `TRNRPT00` JCL with the two date parms substituted) are written one by one to TDQ `JOBS` until a line equal to `/*EOF` or blank is reached (that terminator line is written too). |
| R-19 | `WRITEQ TD` RESP ≠ NORMAL | `ERR-FLG='Y'`; `Unable to Write TDQ (JOBS)...`; send. |

Embedded JCL (`JOB-DATA`): `//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,` … `JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')`; the two
`PARM-START-DATE-n`/`PARM-END-DATE-n` pairs are the in-stream `DATEPARM`-equivalent values consumed by `CBTRN03C` (see batch
baseline `TRANREPT`, frozen at `2022-01-01`..`2022-07-06`).

## RETURN-TO-PREV-SCREEN / SEND-TRNRPT-SCREEN

| # | Given | Then |
|---|---|---|
| R-20 | Return | Blank target → `COSGN00C`; from-fields `CR00`/`CORPT00C`, context 0; `XCTL ... COMMAREA`. |
| R-21 | Send | Standard header (`CR00`, `CORPT00C`); `SEND MAP('CORPT0A') MAPSET('CORPT00') CURSOR` (+`ERASE` unless `SEND-ERASE-NO`, which is never set in this program); then `RETURN TRANSID('CR00') COMMAREA`. |
