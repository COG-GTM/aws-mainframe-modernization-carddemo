# CBACT04C — Interest calculation (job INTCALC, Java STEP15)

Source: `app/cbl/CBACT04C.cbl`; JCL `app/jcl/INTCALC.jcl` (`STEP15 EXEC PGM=CBACT04C,PARM='2022071800'`). Files:
`TCATBALF` KSDS opened **INPUT**, sequential access (`CVTRA01Y`, 50, key `9(11)` + `X(02)` + `9(04)`); `XREFFILE`
KSDS input, random access by the **alternate key** `FD-XREF-ACCT-ID` (`CVACT03Y`, 50; the JCL's `XREFFIL1` AIX path);
`DISCGRP` KSDS input, random (`CVTRA02Y`, 50, key `X(10)` group + `X(02)` type + `9(04)` category); `ACCTFILE` KSDS
**I-O**, random (`CVACT01Y`, 300, key `9(11)`); `TRANSACT` sequential **OUTPUT**, new GDG generation
`AWS.M2.CARDDEMO.SYSTRAN(+1)`, `DISP=(NEW,CATLG,DELETE)`, `RECFM=F LRECL=350` (`CVTRA05Y`). All amounts are signed
zoned decimal with 2 decimals; no COMP-3, no `ROUNDED` anywhere (`BigDecimal`, `RoundingMode.DOWN`, ADR-0004/0005).
Written from the source; checked against `docs/validation/baseline/INTCALC` (GnuCOBOL 3.1.2, PARM `2022071800`, clock
2022-07-06, ADR-0014). Java: `com.carddemo.batch.intcalc.Cbact04c`, job `cbact04c`, stream `intcalc`
(`IntcalcJobConfiguration`).

## Main loop (l.180–232)

| # | Given | Then |
|---|---|---|
| R-1 | Start | `START OF EXECUTION OF PROGRAM CBACT04C`; open TCATBALF, XREFFILE, DISCGRP, ACCTFILE, TRANSACT in that order; a non-`00` status → message + `FILE STATUS IS: NNNN…` + `ABENDING PROGRAM`, abend U999 (Java RC 16). Messages: `ERROR OPENING TRANSACTION CATEGORY BALANCE`, `ERROR OPENING CROSS REF FILE` **followed by the status on the same line**, `ERROR OPENING DALY REJECTS FILE` (sic — the DISCGRP open reuses CBTRN02C's text, kept), `ERROR OPENING ACCOUNT MASTER FILE`, `ERROR OPENING TRANSACTION FILE`. |
| R-2 | Each TCATBALF record in key order (status `00`; `10` = end; other → `ERROR READING TRANSACTION CATEGORY FILE` + abend) | Count it (`WS-RECORD-COUNT`, never displayed) and `DISPLAY TRAN-CAT-BAL-RECORD` — the full 50-byte image, FILLER included. |
| R-3 | `TRANCAT-ACCT-ID NOT = WS-LAST-ACCT-NUM` (account break; `WS-LAST-ACCT-NUM` starts as spaces, so the first record always breaks) | If not the first break: `1050-UPDATE-ACCOUNT` for the **previous** account (R-11). Then `WS-TOTAL-INT` ← 0, remember the account, read it (R-4) and its card (R-5). |
| R-4 | `READ ACCTFILE` by `TRANCAT-ACCT-ID` INVALID KEY | `ACCOUNT NOT FOUND: ` + the 11-digit key, then (status 23 ≠ `00`) `ERROR READING ACCOUNT FILE` + status + abend. |
| R-5 | `READ XREFFILE KEY IS FD-XREF-ACCT-ID` | The first card of the account on the AIX (lowest card number in primary-key order; Java table mode: `CardXrefRepository.findFirstByAcctIdOrderByCardNumAsc`, file mode: `XrefByAccount`). INVALID KEY → `ACCOUNT NOT FOUND: ` + key, `ERROR READING XREF FILE` + status + abend. |
| R-6 | Every record (after any break) | Rate lookup (R-7/R-8); `DIS-INT-RATE NOT = 0` → interest (R-9) and transaction (R-10); `1400-COMPUTE-FEES` is an empty paragraph ("To be implemented"). A **zero rate writes nothing** and adds nothing, but the account is still rewritten at its break (cycle reset). |
| R-12 | End of file | **The last account is never rewritten.** `END-OF-FILE` is set to `Y` inside the loop body, the `PERFORM UNTIL` test then exits, and the `ELSE PERFORM 1050-UPDATE-ACCOUNT` branch (meant as the last-record flush) is unreachable. Its interest transactions are written, but its balance, cycle credit and debit stay as read. Reproduced (baseline account 50 is unchanged); a business fix needs a decision and a new baseline. |
| R-13 | Close | Close the five files (`ERROR CLOSING …` + abend on a bad status); `END OF EXECUTION OF PROGRAM CBACT04C`; RC 0. No counts are displayed. |

## Disclosure group (1200-GET-INTEREST-RATE / 1200-A-GET-DEFAULT-INT-RATE, l.415–460)

| # | Given | Then |
|---|---|---|
| R-7 | `READ DISCGRP` by `ACCT-GROUP-ID` + `TRANCAT-TYPE-CD` + `TRANCAT-CD` | Found → that rate. INVALID KEY (23) → `DISCLOSURE GROUP RECORD MISSING`, `TRY WITH DEFAULT GROUP CODE`, then R-8. Any other non-`00` status → `ERROR READING DISCLOSURE GROUP FILE` + abend. |
| R-8 | Group `DEFAULT` + same type + category | Found → that rate; not found → `ERROR READING DEFAULT DISCLOSURE GROUP` + `FILE STATUS IS: NNNN0023` + abend. Same lookup as `DisclosureGroupRepository.findWithDefault`; the Java program does the two reads itself so the two SYSOUT lines appear between them, in table mode through `findById`. |

All 50 sample accounts have a blank `ACCT-GROUP-ID`, so every TCATBALF row of the baseline takes the DEFAULT path
(two SYSOUT lines per row).

## Interest and transaction (1300-COMPUTE-INTEREST / 1300-B-WRITE-TX, l.462–515)

| # | Given | Then |
|---|---|---|
| R-9 | `COMPUTE WS-MONTHLY-INT = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200` (target `S9(09)V99`, no `ROUNDED`, no `ON SIZE ERROR`) | The exact quotient is **truncated toward zero** to 2 decimals, and high-order digits beyond 9 are lost: 1164.87 × 15.00 / 1200 = 14.560875 → **14.56**; −1164.87 → **−14.56**; 0.07 × 15 → 0.00; 999999999.99 × 9999.99 / 1200 → 333324999.91 (checked with GnuCOBOL 3.1.2). `ADD WS-MONTHLY-INT TO WS-TOTAL-INT` (same PIC, truncated). Negative balances give negative interest. |
| R-10 | Each interest | `WS-TRANID-SUFFIX 9(06)` += 1 (counted over the whole run, wraps at 1 000 000); `TRAN-ID` = `PARM-DATE X(10)` + suffix, e.g. `2022071800000001`. Type `01`, category `0005`, source `System`, description `Int. for a/c ` + `ACCT-ID 9(11)`, amount = R-9, merchant id 0, merchant name/city/ZIP spaces, card = R-5, `TRAN-ORIG-TS` = `TRAN-PROC-TS` = now as `YYYY-MM-DD-HH.MM.SS.hh0000` (`Z-GET-DB2-FORMAT-TIMESTAMP`; golden clock → `2022-07-06-00.00.00.000000`). FILLER `X(20)` = spaces. Write error → `ERROR WRITING TRANSACTION RECORD` + abend. |

PARM in Java: job parameter `PARM` (`--PARM=` / `--STEP15.PARM=`), else `carddemo.baseline.intcalc-parm-date` (the
`golden` profile pins `2022071800`), else the run date as `yyyyMMdd00`. Shorter values are padded with spaces, longer
ones cut to 10, as `PARM-DATE X(10)`.

## Account update (1050-UPDATE-ACCOUNT, l.350–370)

| # | Given | Then |
|---|---|---|
| R-11 | Account break (R-3) | `ACCT-CURR-BAL` += `WS-TOTAL-INT` (target `S9(10)V99`), `ACCT-CURR-CYC-CREDIT` ← 0, `ACCT-CURR-CYC-DEBIT` ← 0, `REWRITE` from `ACCOUNT-RECORD`; bad status → `ERROR RE-WRITING ACCOUNT FILE` + abend. Accounts with no TCATBALF rows are never read or rewritten (their cycle totals are kept). TCATBALF itself is not updated (input only). |

## FILLER, units of work and abends (Java)

- File mode: ACCTFILE is rewritten from the bytes of the record last read (`KeyedDataset`), so `ACCT-FILLER X(178)` is
  preserved byte for byte; TRANSACT records are built fresh (FILLER spaces), as the COBOL `TRAN-RECORD` working-storage.
- Table mode: tables hold no FILLER (ADR-0011), so the DISPLAYed TCATBALF images end in spaces instead of the sample's
  22 zeros; `compare_intcalc.py` reports these as FILLER-only differences.
- Table mode is one database transaction for the step: an abend rolls back every account rewrite and no SYSTRAN
  generation is catalogued (`DISP=(NEW,CATLG,DELETE)`; an explicit `--TRANSACT=<path>` file is deleted). File mode
  keeps the ACCTFILE rewrites issued before the abend (VSAM).
  Restarts are refused (`preventRestart`); rerun as a new instance after restoring the inputs.
- `batch_run`: read count = TCATBALF records, write count = SYSTRAN records (baseline: 100 / 50, RC 0).

## Baseline run order and start state

`scripts/baseline/baseline.py` runs POSTTRAN before INTCALC, so the baseline INTCALC read the **POSTTRAN after-images**
of ACCTDATA and TCATBALF (100 TCATBALF rows: types 01 and 03, category 0001) plus the `app/data/ASCII` CARDXREF and
DISCGRP. `scripts/batch/run_intcalc.sh` starts from the same state without running POSTTRAN: file mode copies the two
after-images from `docs/validation/baseline/POSTTRAN`; table mode runs `initial-load` (EBCDIC samples) and then
`--job=repro` of the two after-images. The EBCDIC DISCGRP record 34 (`DEFAULT`/`07`/`0001`, rate 15.00 vs 0.00 in
ASCII) is loaded but no TCATBALF row has type 07, so no lookup reaches it and it cannot change SYSTRAN or ACCTDATA;
`compare_intcalc.py` recomputes and reports that count on every run
(`scripts/batch/intcalc-table-mode-expected-diffs.txt`).
