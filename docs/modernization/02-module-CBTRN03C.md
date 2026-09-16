# 2c. Module documentation — CBTRN03C (transaction detail report) and the TRANREPT SORT step

Source: `app/cbl/CBTRN03C.cbl` (649 lines), `app/cpy/CVTRA07Y.cpy` (report layouts).
Job: `app/jcl/TRANREPT.jcl` — STEP05R REPRO (lines 23-33), STEP05R SORT (37-55), STEP10R
CBTRN03C (59-80). No scheduler definition for TRANREPT exists in `app/scheduler/`.

## Business purpose (lines 3-6, 159-215)

Produces a 133-column printable report of posted transactions within a date range, grouped by
card, with per-page totals, per-account totals and a grand total. Each detail line joins the
transaction to its account (via card cross-reference) and to the type and category descriptions.

## The two JCL pre-steps (TRANREPT.jcl)

1. **REPRO** (`PROC=REPROC` → `app/proc/TRANREPT.prc`, control `app/ctl/REPROCT.ctl`):
   copies `TRANSACT.VSAM.KSDS` to sequential `TRANSACT.BKUP(+1)` in primary-key (TRAN-ID) order.
2. **DFSORT** (lines 37-55):
   * `SYMNAMES`: `TRAN-CARD-NUM,263,16,ZD` (1-based position 263 = copybook offset 262),
     `TRAN-PROC-DT,305,10,CH` (offset 304 = first 10 bytes of `TRAN-PROC-TS`),
     `PARM-START-DATE,C'2022-01-01'`, `PARM-END-DATE,C'2022-07-06'`.
   * `INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,AND,TRAN-PROC-DT,LE,PARM-END-DATE)` —
     inclusive on both ends, character comparison.
   * `SORT FIELDS=(TRAN-CARD-NUM,A)` — ascending; declared `ZD` although the field is
     `PIC X(16)`. For all-digit card numbers ZD and CH ordering agree; non-digit card numbers
     would sort differently (OQ-13). No `EQUALS` option → order of records with the same card
     number is not guaranteed (OQ-14).
   * Output `TRANSACT.DALY(+1)`.

The hard-coded date window in the SORT and the `DATEPARM` file read by the program are two
independent date ranges. If they disagree the program's range is applied *after* the SORT's
(OQ-15).

## Inputs and outputs (FILE-CONTROL 28-57; JCL 65-80)

| DD | Org / access | Open (lines) | Layout | LRECL |
|---|---|---|---|---|
| `TRANFILE` | sequential | INPUT (376-393) | CVTRA05Y `TRAN-RECORD` | 350 |
| `CARDXREF` | indexed, random, key card | INPUT (412-429) | CVACT03Y | 50 |
| `TRANTYPE` | indexed, random, key `FD-TRAN-TYPE` X(2) | INPUT (430-447) | CVTRA03Y | 60 |
| `TRANCATG` | indexed, random, key type(2)+cat(4) | INPUT (448-465) | CVTRA04Y | 60 |
| `DATEPARM` | sequential | INPUT (466-483) | `FD-DATEPARM-RECORD X(80)` → `WS-DATEPARM-RECORD`: `WS-START-DATE X(10)` @0, FILLER X(1) @10, `WS-END-DATE X(10)` @11 (122-126) | 80 |
| `TRANREPT` | sequential | OUTPUT (394-411) | CVTRA07Y lines | 133 |

## Report record layouts (CVTRA07Y, all 133 bytes)

| Record | Content |
|---|---|
| `REPORT-NAME-HEADER` | `X(38)` short name, `X(41)` long name, `X(12)` date header, `REPT-START-DATE X(10)`, `' to '`, `REPT-END-DATE X(10)` (dates filled at 279-280) |
| `WS-BLANK-LINE` | spaces |
| `TRANSACTION-HEADER-1` | column captions (fillers X(17), X(12), X(19), X(35), X(14), X(1), X(16)) |
| `TRANSACTION-HEADER-2` | `X(133) VALUE ALL '-'` |
| `TRANSACTION-DETAIL-REPORT` | `TRANS-ID X(16)`, sp, `ACCOUNT-ID X(11)`, sp, `TYPE-CD X(2)`, `-`, `TYPE-DESC X(15)`, sp, `CAT-CD 9(4)`, `-`, `CAT-DESC X(29)`, sp, `SOURCE X(10)`, 4 sp, `AMT PIC -ZZZ,ZZZ,ZZZ.ZZ` (15), 2 sp |
| `REPORT-PAGE-TOTALS` | `X(11)` caption, `X(86)` dots, `REPT-PAGE-TOTAL PIC +ZZZ,ZZZ,ZZZ.ZZ`, remainder |
| `REPORT-ACCOUNT-TOTALS` | `X(13)` caption, `X(84)` dots, `REPT-ACCOUNT-TOTAL +ZZZ,ZZZ,ZZZ.ZZ` |
| `REPORT-GRAND-TOTALS` | `X(11)` caption, `X(86)` dots, `REPT-GRAND-TOTAL +ZZZ,ZZZ,ZZZ.ZZ` |

Edited pictures: `-ZZZ,ZZZ,ZZZ.ZZ` is a floating minus (blank for positive), `+ZZZ,ZZZ,ZZZ.ZZ`
a floating plus/minus. Every digit position is `Z`, so by the standard editing rule a value of
zero renders the **entire 15-character field as spaces** (decimal point included); `0.05`
renders as `            .05`. The Java `CobolEditedAmount` encodes this rule and is unit-tested;
it has not been confirmed against Enterprise COBOL output (see 05-equivalence-evidence.md).

## Processing flow (159-215)

1. Open six files (161-166), `0550-DATEPARM-READ` (168; 220-247): status `00` displays the
   range; `10` (empty file) sets `END-OF-FILE='Y'` so the main loop never runs and an **empty
   report file is produced with no headers** (OQ-16); other → abend.
2. Loop until EOF (170-213). `1000-TRANFILE-GET-NEXT` (248-273): `10` → EOF, other non-`00` →
   abend.
3. Date filter (173-178):
   ```cobol
   IF TRAN-PROC-TS (1:10) >= WS-START-DATE AND TRAN-PROC-TS (1:10) <= WS-END-DATE
      CONTINUE
   ELSE
      NEXT SENTENCE
   END-IF
   ```
   `NEXT SENTENCE` transfers control to the statement after the next period. The next period
   is the one terminating `END-PERFORM.` (line 213). **An out-of-range record therefore ends
   the whole read loop**; the program closes files and exits without page/grand totals.
   Because the SORT step has already removed out-of-range records, this branch is normally
   never taken in the JCL flow — but it is what the program does. OQ-17 (probable intent: skip
   the record).
4. Not EOF (179-197): `DISPLAY TRAN-RECORD`; on card change (181-188) write account totals for
   the previous card unless first record, then look up the XREF by card (abend on miss);
   look up type and category (189-195; abend on miss); `1100-WRITE-TRANSACTION-REPORT` (196).
5. EOF (198-206): `ADD TRAN-AMT TO WS-PAGE-TOTAL WS-ACCOUNT-TOTAL` — **adds the last record's
   amount a second time** (the record area still holds the last record after the EOF read on
   Enterprise COBOL; behaviour is formally undefined, OQ-18) — then page totals and grand totals.
   **No account-total line is written for the last card** (OQ-19), and the double-counted amount
   flows into the grand total via `1110-WRITE-PAGE-TOTALS` (297).

## Pagination and totals (274-375)

* `WS-PAGE-SIZE = 20` (line 131, `COMP-3`), `WS-LINE-COUNTER` counts *every* line written
  (headers, totals, separators, details) — not detail rows.
* First call writes headers (275-280; `1120-WRITE-HEADERS` 324-342 = 4 lines).
* Before each detail: `IF MOD(WS-LINE-COUNTER, 20) = 0` → page totals (2 lines) + headers
  (4 lines) (282-285).
* Then `ADD TRAN-AMT TO WS-PAGE-TOTAL WS-ACCOUNT-TOTAL` and the detail line (287-289).
* `1110-WRITE-PAGE-TOTALS` (293-304): total line, `ADD WS-PAGE-TOTAL TO WS-GRAND-TOTAL`, reset,
  separator line. Grand total is therefore the sum of page totals, and only page totals ever
  feed it.
* `1120-WRITE-ACCOUNT-TOTALS` (306-316): total line + separator; reset account total.
* `1120-WRITE-DETAIL` (361-374): `INITIALIZE` then field moves; `TRAN-REPORT-AMT` receives
  `TRAN-AMT` via edited MOVE.
* Duplicate paragraph names by prefix (`1110-WRITE-PAGE-TOTALS` / `1110-WRITE-GRAND-TOTALS`,
  `1120-WRITE-ACCOUNT-TOTALS` / `1120-WRITE-HEADERS` / `1120-WRITE-DETAIL`) are cosmetic.

## Error and abend paths

| Condition | Lines | Message | Outcome |
|---|---|---|---|
| Any OPEN ≠ `00` | 376-483 | `ERROR OPENING <file>` | abend U0999 |
| DATEPARM READ ∉ {`00`,`10`} | 220-247 | `ERROR READING DATEPARM FILE` | abend |
| TRANFILE READ ∉ {`00`,`10`} | 248-273 | `ERROR READING TRANSACTION FILE` | abend |
| XREF miss | 484-493 | `INVALID CARD NUMBER : nnnn`, status shown as 0023 | abend |
| TRANTYPE miss | 494-503 | `INVALID TRANSACTION TYPE : nn` | abend |
| TRANCATG miss | 504-513 | `INVALID TRAN CATG KEY : nnnnnn` | abend |
| REPTFILE WRITE ≠ `00` | 343-360 | `ERROR WRITING REPTFILE` | abend |
| Any CLOSE ≠ `00` | 514-625 | `ERROR CLOSING <file>` | abend |

A lookup miss aborts the whole report, mid-page, with the partial report left in `TRANREPT(+1)`.
No `RETURN-CODE` is set on the success path.
