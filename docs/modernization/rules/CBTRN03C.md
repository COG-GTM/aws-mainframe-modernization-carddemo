# CBTRN03C — Daily transaction report (job TRANREPT, step STEP10R; Java STEP15)

Source: `app/cbl/CBTRN03C.cbl`, report layouts `app/cpy/CVTRA07Y.cpy`; JCL `app/jcl/TRANREPT.jcl` + `app/proc/TRANREPT.prc`.
Files: `TRANFILE` sequential input (`CVTRA05Y`, 350) = `TRANSACT.DALY(+1)`, the STEP10 SORT extract; `CARDXREF` KSDS random
by card number (`CVACT03Y`); `TRANTYPE` KSDS random by type (`CVTRA03Y`); `TRANCATG` KSDS random by type + category
(`CVTRA04Y`); `DATEPARM` sequential, one record `start-date end-date` (`X(10) X(01) X(10)`); `REPTFILE` (DD
`TRANREPT`) sequential output, new generation `AWS.M2.CARDDEMO.TRANREPT(+1)`, **133-byte** records. No `ROUNDED`, all
amounts `S9(09)V99` (`BigDecimal`). Written from the source; checked against `docs/validation/baseline/TRANREPT`
(GnuCOBOL 3.1.2, DATEPARM `2022-01-01 2022-07-06`, clock 2022-07-06). Java: `com.carddemo.batch.tranrept.Cbtrn03c`, job
`cbtrn03c`, stream `tranrept` (`TranreptJobConfiguration`).

## The job (TRANREPT.jcl)

The JCL has two steps named `STEP05R` (the `REPROC` backup and the SORT); Java needs unique step names, so the stream
numbers them `STEP05` / `STEP10` / `STEP15` (step-qualified parameters: `--STEP05.FILEIN=`, `--STEP10.SORTIN=`,
`--STEP15.DATEPARM=`, `--STEP15.SYSOUT=` …).

| JCL step | Legacy | Java step / job |
|---|---|---|
| `STEP05R` (PROC `REPROC`) | IDCAMS `REPRO` TRANSACT KSDS → `TRANSACT.BKUP(+1)` | `STEP05` / `reproc`: unload the `transaction` table (or `--STEP05.FILEIN`) in key order to a dated `TRANSACT.BKUP` generation (`batch_output_file`) |
| `STEP05R` (`PGM=SORT`) | `INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,AND,TRAN-PROC-DT,LE,PARM-END-DATE)` with `TRAN-PROC-DT = 305,10,CH` (= `TRAN-PROC-TS(1:10)`), `SORT FIELDS=(TRAN-CARD-NUM,A)` (`263,16,ZD`) → `TRANSACT.DALY(+1)` | `STEP10` / `tranrept-sort`: filters and stable-sorts the `TRANSACT.BKUP` generation written by this run's `STEP05` (bound to that step's output, not a `(0)` catalogue lookup, so a backdated run never reads another run's backup), or `--STEP10.SORTIN=<unload>`. Run standalone with no SORTIN it is the date-range query `TransactionRepository.findByProcDateWindow` on `transaction` (`substr(proc_ts,1,10) collate "C" between`, ordered by card number then TRAN-ID = the same selection; `HousekeepingJobIT` checks the two agree). Window = DATEPARM, else `carddemo.baseline.tranrept-start-date` / `-end-date` (`golden` profile) |
| `STEP10R` | `CBTRN03C` | `STEP15` / `cbtrn03c` (this document) → dated `TRANREPT` generation |

## Rules

| # | Given | Then |
|---|---|---|
| R-1 | Start | `START OF EXECUTION OF PROGRAM CBTRN03C`; open TRANFILE, REPTFILE, CARDXREF, TRANTYPE, TRANCATG, DATEPARM in that order; a bad status → `ERROR OPENING …` + `FILE STATUS IS: NNNN…` + `ABENDING PROGRAM`, abend U999 (Java RC 16). |
| R-2 | Read DATEPARM (`0550-DATEPARM-READ`) | `00` → `Reporting from <start> to <end>`; `10` → end of file is set (nothing is reported, no totals); other → `ERROR READING DATEPARM FILE` + abend. |
| R-3 | **Date filter** | A record is reported when `TRAN-PROC-TS (1:10) >= WS-START-DATE AND <= WS-END-DATE` — the **processing** timestamp, not `TRAN-ORIG-TS`. Byte-wise (alphanumeric) comparison; Java compares the encoded bytes of the record's code page. Same field the SORT `INCLUDE` uses (`TRAN-PROC-DT`, position 305). |
| R-4 | A record outside the window | `NEXT SENTENCE` jumps past the next period, which is the one after `END-PERFORM`: **the loop ends** — no further records, no page total and no grand total, and the files are closed normally (RC 0). Confirmed with GnuCOBOL 3.1.2. In TRANREPT this cannot happen (the SORT step already dropped those records); it only matters for a TRANFILE that was not extracted. |
| R-5 | Each record in the window | `DISPLAY TRAN-RECORD` (the 350-byte image). |
| R-6 | `TRAN-CARD-NUM` differs from the previous record's (`WS-CURR-CARD-NUM`, starts as spaces) | If not the first record: write the **Account Total** of the previous card (R-11). The "account" total is really a **per-card** subtotal: it breaks on card number, not on `XREF-ACCT-ID` (the sample has one card per account, so the two coincide there). Then `READ CARDXREF` by card number; INVALID KEY → `INVALID CARD NUMBER : <card>`, `FILE STATUS IS: NNNN0023`, abend. |
| R-7 | Every record | `READ TRANTYPE` by `TRAN-TYPE-CD` (INVALID KEY → `INVALID TRANSACTION TYPE : ` + key, abend) and `READ TRANCATG` by type + `TRAN-CAT-CD` (INVALID KEY → `INVALID TRAN CATG KEY : ` + key, abend). |
| R-8 | First detail (`WS-FIRST-TIME = 'Y'`) | Move the DATEPARM dates into the header and write the page headers (R-12). |
| R-9 | Before each detail: `FUNCTION MOD(WS-LINE-COUNTER, 20) = 0` | Write the **Page Total** (R-10) and the page headers (R-12). `WS-LINE-COUNTER` counts **every** line written (headers, details, totals and their separator lines), so pages are not 20 details long and the break only happens when the counter lands on a multiple of 20. |
| R-10 | Page total (`1110-WRITE-PAGE-TOTALS`) | `Page Total` + dots + `REPT-PAGE-TOTAL` (`+ZZZ,ZZZ,ZZZ.ZZ`), then a line of 133 `-`; the page total is added to `WS-GRAND-TOTAL` and reset. 2 lines. |
| R-11 | Account total (`1120-WRITE-ACCOUNT-TOTALS`) | `Account Total` + dots + `REPT-ACCOUNT-TOTAL` (`+ZZZ,ZZZ,ZZZ.ZZ`), then 133 `-`; reset. 2 lines. |
| R-12 | Page headers (`1120-WRITE-HEADERS`) | 4 lines: `REPORT-NAME-HEADER` (`DALYREPT` in X(38), `Daily Transaction Report` in X(41), `Date Range: `, start, ` to `, end), a blank line, `TRANSACTION-HEADER-1` (column titles), `TRANSACTION-HEADER-2` (133 `-`). No page number. |
| R-13 | Detail (`1120-WRITE-DETAIL`) | `TRAN-ID` X(16), space, `XREF-ACCT-ID` X(11), space, type X(02) `-` type description X(15), space, category 9(04) `-` category description X(29), space, `TRAN-SOURCE` X(10), 4 spaces, `TRAN-AMT` as `-ZZZ,ZZZ,ZZZ.ZZ` (sign in front, blank when positive), 2 spaces = 114 bytes; the amount is added to the page and account totals. |
| R-14 | End of file (`1000-TRANFILE-GET-NEXT` status `10`) | The `ELSE` branch: `DISPLAY 'TRAN-AMT ' TRAN-AMT`, `DISPLAY 'WS-PAGE-TOTAL' WS-PAGE-TOTAL` (no space), then **the last record's `TRAN-AMT` is added once more** to the page and account totals (the record area still holds it), the page total (R-10) and the **Grand Total** (`Grand Total` + dots + `+ZZZ,ZZZ,ZZZ.ZZ`, 1 line) are written. **The last card's Account Total is never written.** Baseline: 50 cards, 49 account totals; the last page total and the grand total include the last amount twice. Reproduced; fixing it needs a business decision and a new baseline. |
| R-15 | Close | Close the six files (`ERROR CLOSING …` + abend); `END OF EXECUTION OF PROGRAM CBTRN03C`; RC 0. |

## Record format

- Every REPTFILE record is **133 bytes** (`FD-REPTFILE-REC PIC X(133)`); shorter report groups are padded with spaces
  (the detail is 114 bytes, the totals 112, the name header 115, the column header 114). Java keeps the 133-byte fixed
  records in the generation; the equivalence check compares lines after trailing-space normalisation and checks the
  length separately.
- Numeric editing: `-ZZZ,ZZZ,ZZZ.ZZ` (details) and `+ZZZ,ZZZ,ZZZ.ZZ` (totals) via `NumericEdited`; the sign occupies the
  first position and the digits are right-aligned, so `-` / `+` is separated from the value by spaces.
- Baseline (DATEPARM `2022-01-01 .. 2022-07-06`, 312 records after COMBTRAN): 519 lines = 18 page-header blocks,
  312 details, 49 account totals, 18 page totals, 1 grand total (`+     79,254.29`).

## Java mapping

`Cbtrn03c.run()` keeps the COBOL control flow (`WS-FIRST-TIME`, `WS-CURR-CARD-NUM`, `WS-LINE-COUNTER`, page/account/
grand totals as `BigDecimal`). CARDXREF/TRANTYPE/TRANCATG are `KeyedDataset`s (table: `card_xref`, `transaction_type`,
`transaction_category`; file: the KSDS unloads); DATEPARM is a file (`--STEP15.DATEPARM`) or, when absent, the golden
window `carddemo.baseline.tranrept-start-date` / `tranrept-end-date`. The report goes to a `BufferedSink`, written as the dated `TRANREPT`
generation only when the step ends normally (`DISP=(NEW,CATLG,DELETE)`).
