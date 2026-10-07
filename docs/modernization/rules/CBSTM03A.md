# CBSTM03A — Account statements, text and HTML (job CREASTMT, step STEP040)

Source: `app/cbl/CBSTM03A.CBL`, copybooks `COSTM01` (TRNX-RECORD), `CVACT03Y` (CARD-XREF-RECORD), `CUSTREC`
(CUSTOMER-RECORD), `CVACT01Y` (ACCOUNT-RECORD); JCL `app/jcl/CREASTMT.JCL`. All file I/O goes through `CBSTM03B`
(`CBSTM03B.md`). Inputs: `TRNXFILE` = TRXFL KSDS (key card + transaction id, 350 bytes), `XREFFILE` = CARDXREF,
`CUSTFILE` = CUSTDATA, `ACCTFILE` = ACCTDATA. Outputs: `STMTFILE` = `STATEMNT.PS`, **80-byte** records; `HTMLFILE` =
`STATEMNT.HTML`, **100-byte** records. No `ROUNDED`; the only arithmetic is `ADD TRNX-AMT TO WS-TOTAL-AMT`.
Written from the source; checked against `docs/validation/baseline/CREASTMT` (GnuCOBOL 3.1.2, clock 2022-07-06, the
build-time patch `00-COMPILE/CBSTM03A.gnucobol.patch`) and against GnuCOBOL runs on synthetic inputs
(`scripts/batch/gen_creastmt_edge_cases.py` → `modernization/carddemo-app/src/test/resources/creastmt/`). Java:
`com.carddemo.batch.creastmt.Cbstm03a`, job `cbstm03a`, stream `creastmt` (`CreastmtJobConfiguration`).

## The job (CREASTMT.JCL)

| JCL step | Legacy | Java step / job |
|---|---|---|
| `DELDEF01` | IDCAMS `DELETE` + `DEFINE CLUSTER` TRXFL `KEYS(32 0) RECORDSIZE(350 350)` | Not a step: TRXFL is a new dated generation every run (ADR-0012), there is nothing to delete. |
| `STEP010` | DFSORT over TRANSACT KSDS: `SORT FIELDS=(263,16,CH,A,1,16,CH,A)` (card number, then TRAN-ID), `OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)` → `TRXFL.SEQ`, LRECL 350 | `STEP010` / `creastmt-sort`: the `transaction` table via `TransactionRepository.findAllForStatements` (`order by card_num collate "C", tran_id collate "C"` = the CH sort in byte order), or `--STEP010.SORTIN=<KSDS unload>` sorted with `Dfsort.ch(263,16)` then `ch(1,16)`. OUTREC: bytes 263–278 (card), then 1–262, then 279–328; 329–350 are spaces (the reformatted record is 328 bytes, SORTOUT is 350). Dated `TRXFL.SEQ` generation. `ICE054I 0 RECORDS - IN: n, OUT: n`. |
| `STEP020` | IDCAMS `REPRO INFILE(TRXFL.SEQ) OUTFILE(TRXFL.VSAM.KSDS)` | `STEP020` / `trxfl-repro`: loads this run's `TRXFL.SEQ` (bound to STEP010's file, `--STEP020.INFILE=` to override) into a dated `TRXFL` generation after checking the 32-byte keys ascend (duplicate → `IDC3316I DUPLICATE RECORD` RC 12; out of sequence → `IDC3314I RECORD OUT OF SEQUENCE` RC 12; nothing is catalogued). `IDC0005I NUMBER OF RECORDS PROCESSED WAS n`. |
| `STEP030` | IEFBR14 `DELETE` of the old `STATEMNT.PS` / `STATEMNT.HTML` | Not a step: both outputs are new dated generations. |
| `STEP040` | `CBSTM03A` | `STEP040` / `cbstm03a` (this document). DDs `--STEP040.TRNXFILE=` (default: this run's `TRXFL`), `XREFFILE` / `CUSTFILE` / `ACCTFILE` (file = KSDS unload; table = `card_xref` / `customer` / `account`). Dated `STATEMNT.PS` and `STATEMNT.HTML` generations, written only when the step ends normally. |

A JCL step that fails stops the stream (`COND` semantics of the harness, ADR-0015): no later step runs, and an abended
STEP040 catalogues neither statement file.

## Rules

| # | Given | Then |
|---|---|---|
| R-1 | Start | z/OS only: the program walks PSA → TCB → TIOT and `DISPLAY`s `Running JCL : <job> Step <step>`, `DD Names from TIOT: ` and one `: <ddname> -- valid UCB` / `-- null UCB` line per DD (the last DD is displayed twice). Java DISPLAYs the first line from the stream's job and step names (`Running JCL : CREASTMT Step STEP040`) and does **not** reproduce the DD list (control-block introspection, no business content). The GnuCOBOL baseline was built with the walk removed and DISPLAYs `Running JCL : CREASTMT  Step STEP040 (TIOT walk bypassed under GnuCOBOL)`; the comparator substitutes exactly that line (R-2 of the equivalence, `compare_creastmt.py`). |
| R-2 | Open | `OPEN OUTPUT STMT-FILE HTML-FILE`, `INITIALIZE WS-TRNX-TABLE WS-TRN-TBL-CNTR`. Then the `ALTER`ed `GO TO` chain (`0000-START`, `WS-FL-DD`): open TRNXFILE and load it completely (R-3), then open XREFFILE, CUSTFILE, ACCTFILE, each via CBSTM03B `O`. RC other than `00`/`04` → `ERROR OPENING <dd>`, `RETURN CODE: <rc>`, R-14. Java: the same order as straight-line calls (no `ALTER`). |
| R-3 | Load TRXFL (`8100-TRNXFILE-OPEN`, `8500-READTRNX-READ`) | First `R`: `00` or `04` accepted, anything else (including `10`, **an empty TRXFL**) → `ERROR READING TRNXFILE`, `RETURN CODE: 10`, R-14 — an empty TRXFL abends instead of producing statements without transactions (confirmed with GnuCOBOL: RC 16 `CEE3ABD`; Java RC 16). Then for each record: same card as the previous one → `TR-CNT + 1`; new card → `WS-TRCT (CR-CNT) = TR-CNT`, `CR-CNT + 1`, `TR-CNT = 1`; store card number in `WS-CARD-NUM (CR-CNT)`, TRAN-ID in `WS-TRAN-NUM (CR-CNT, TR-CNT)`, the 318-byte rest in `WS-TRAN-REST (CR-CNT, TR-CNT)`. Next `R`: `00` → repeat, `10` → `WS-TRCT (CR-CNT) = TR-CNT` and continue with R-2, other → `ERROR READING TRNXFILE` + R-14. Subsequent reads do not accept `04`. |
| R-4 | **Table limits** | `WS-TRNX-TABLE`: `WS-CARD-TBL OCCURS 51` × (`WS-CARD-NUM X(16)` + `WS-TRAN-TBL OCCURS 10` × (`X(16)` + `X(318)`)) = 51 × 3356 bytes; `WS-TRN-TBL-CNTR`: 51 × `S9(4) COMP`. Subscripts are not checked. A card with **more than 10 transactions** writes the 11th onwards over the next card's slot (card number + first transactions); the next card then overwrites that storage again, so the first card's statement prints `WS-TRCT` lines of which lines 11+ are the next card's slot read back as transactions (the next card number where the TRAN-ID should be, the rest shifted by 16 bytes, amounts read from non-numeric bytes). Confirmed with GnuCOBOL (`creastmt/overflow`: a card with 12 transactions prints 12 lines, lines 11 and 12 garbled, both `$         .00`, and its real 11th/12th amounts are lost from the total). **Java reproduces this overlay byte for byte inside the 51 × 10 table.** An entry beyond the end of the table (the 52nd card, or the 11th+ transaction of card 51) would overwrite `WS-TRN-TBL-CNTR` and other working storage in COBOL (undefined, compiler/layout dependent); Java DISPLAYs `WS-TRNX-TABLE OVERFLOW: CARD <n> TRANSACTION <n> IS OUTSIDE THE 51 x 10 TABLE` + `ABENDING PROGRAM` and abends (RC 16, no statement files). Deliberate, documented deviation: the legacy result there is not reproducible and silently corrupt output is worse than a stop. The sample data has 50 cards with at most 7 transactions each. |
| R-5 | Main loop (`1000-MAINLINE`) | Sequential `R` on XREFFILE until `10` (`END-OF-FILE = 'Y'`); other non-`00` → `ERROR READING XREFFILE` + R-14. **One statement per CARDXREF record, i.e. per card**, in card-number order: a customer or account with two cards gets two statements. |
| R-6 | Each XREF | `K` on CUSTFILE with `XREF-CUST-ID` (key length 9) and on ACCTFILE with `XREF-ACCT-ID` (11); not `00` → `ERROR READING CUSTFILE` / `ERROR READING ACCTFILE` + `RETURN CODE: 23` + R-14. Then R-7 (header), `WS-TOTAL-AMT = 0`, R-9 (transactions), R-10 (totals). |
| R-7 | Statement header (`5000-CREATE-STATEMENT`) | `INITIALIZE STATEMENT-LINES` (the variable fields become spaces/zero), `ST-LINE0`; HTML header (R-12); `ST-NAME` = `STRING CUST-FIRST-NAME DELIMITED BY ' ', ' ', CUST-MIDDLE-NAME DELIMITED BY ' ', ' ', CUST-LAST-NAME DELIMITED BY ' ', ' '` — **each name part is cut at its first space**, so a two-word first/last name keeps only its first word; `ST-ADD1/2` = address lines 1/2 (X(50)); `ST-ADD3` = line 3, state, country, ZIP the same way (each cut at its first space, single spaces between); `ST-ACCT-ID` = ACCT-ID; `ST-CURR-BAL` = ACCT-CURR-BAL; `ST-FICO-SCORE` = CUST-FICO-CREDIT-SCORE. Then the HTML name/address/basic block (R-12) **before** the text lines 1–15 (each file keeps its own order). |
| R-8 | `ST-CURR-BAL PIC 9(9).99-` from `S9(10)V99` | The **high-order digit is dropped** (balances ≥ 1,000,000,000.00 print wrong); trailing `-` for negatives, space otherwise; no zero suppression (`000000492.00 `). |
| R-9 | Transactions (`4000-TRNXFILE-GET`) | `PERFORM VARYING CR-JMP FROM 1 UNTIL CR-JMP > CR-CNT OR WS-CARD-NUM (CR-JMP) > XREF-CARD-NUM` (byte comparison): for the table card equal to the XREF card, every `TR-JMP` 1..`WS-TRCT` in load order (= TRAN-ID order) → R-11 and `ADD TRNX-AMT TO WS-TOTAL-AMT`. A card with no TRXFL entry gets the header, the column titles and a zero total (`creastmt/no-transactions`: cards below, between and above the TRXFL cards). A TRXFL card with no XREF record is never printed. The early stop relies on both files being in card order (TRXFL from STEP010, XREF a KSDS). |
| R-10 | Totals | `ST-LINE12` (80 `-`), `ST-LINE14A` = `Total EXP:` + 56 spaces + `$` + `ST-TOTAL-TRAMT` (`Z(9).99-`, so zero is `         .00 `), `ST-LINE15` (`*` ×32, `END OF STATEMENT`, `*` ×32); HTML `<tr>`, `HTML-L10`, `HTML-L75` (`<h3>End of Statement</h3>`), `</td>`, `</tr>`, `</table>`, `</body>`, `</html>` — **every statement is a complete HTML document**, so STATEMNT.HTML is a concatenation of 50 documents. `WS-TOTAL-AMT` is `COMP-3 S9(9)V99` without `ON SIZE ERROR`: high-order truncation (Java `CobolNumeric.truncate(…, 11, 2)`). The total only sums the printed lines; it is an "expense" total in name only (credits are subtracted). |
| R-11 | Transaction line (`6000-WRITE-TRANS`, `ST-LINE14`) | TRAN-ID X(16), space, `TRNX-DESC` cut to X(49), `$`, `TRNX-AMT` as `Z(9).99-` (14 bytes, trailing sign) = 80 bytes. HTML: `<tr>`, `HTML-L58` (25% cell), `<p>` + TRAN-ID + `</p>`, `</td>`, `HTML-L61` (55%), `<p>` + the 49-byte description (trailing spaces included) + `</p>`, `</td>`, `HTML-L64` (20%), `<p>` + the edited amount (leading blanks included) + `</p>`, `</td>`, `</tr>`. |
| R-12 | HTML (`5100-WRITE-HTML-HEADER`, `5200-WRITE-HTML-NMADBS`) | Fixed lines are level-88 values of `HTML-FIXED-LN X(100)` (`SET … TO TRUE` + `WRITE … FROM`), so every record is the literal padded to 100 bytes. Variable lines are built with `STRING … DELIMITED BY '*'` (none of the literals or data contain `*`, so it means "whole field") into a buffer cleared with `MOVE SPACES` first: `<h3>Statement for Account Number: ` + ACCT-ID in X(20) + `</h3>` (`HTML-L11`); name line `<p style="font-size:16px">` + `L23-NAME` = **`ST-NAME` cut to 50 bytes** then `DELIMITED BY '  '` (two spaces) + two spaces + `</p>`; address lines `<p>` + `ST-ADDn DELIMITED BY '  '` + two spaces + `</p>`; `<p>Account ID         : ` / `Current Balance    : ` / `FICO Score         : ` + the 20 / 13 / 20-byte text field (trailing spaces kept) + `</p>`. Bank address (`Bank of XYZ`, `410 Terry Ave N`, `Seattle WA 99999`), table styles and column titles are literals. No HTML escaping of data (`&`, `<` in names or descriptions would be written raw; none in the sample). |
| R-13 | End | Close TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE (`C`; not `00`/`04` → `ERROR CLOSING <dd>` + R-14), `CLOSE STMT-FILE HTML-FILE`, `GOBACK` (RC 0). No end-of-job DISPLAY, no counts. |
| R-14 | Abend (`9999-ABEND-PROGRAM`) | `ABENDING PROGRAM`, `CALL 'CEE3ABD'` without an abend code. Java: `AbendException` → step RC 16; the buffered statement files are discarded (no generation). |

## Deviations

| # | Legacy | Java | Why / test |
|---|---|---|---|
| D-1 | R-12: names and addresses are written into STATEMNT.HTML raw (`&`, `<`, `>` in customer data would become markup). | Default unchanged (`carddemo.batch.creastmt.html-escape=false`, byte parity, golden set). With `true`, the name line and the three address lines are HTML-escaped (`&` `<` `>` → entities; quotes need no escaping in text content) after the `DELIMITED BY '  '` cut; an entity that would not fit in the 100-byte record is dropped whole, never cut. STATEMNT.PS, the account/balance/FICO lines and the transaction lines are not affected. | s6.4 hardening (stored XSS in a statement opened in a browser). `Cbstm03aHtmlEscapeTest`: markup raw when off, escaped when on, only those lines change, sample data identical in both modes. |

## Record formats

- `STMTFILE` 80 bytes. Per statement: `ST-LINE0`, name (X(75) + 5), address 1, address 2 (X(50) + 30), address 3 (X(80)),
  80 `-`, `Basic Details` centred (33 + 14 + 33), 80 `-`, `Account ID         :` + X(20) + 40,
  `Current Balance    :` + `9(9).99-` + 47, `FICO Score         :` + X(20) + 40, 80 `-`, `TRANSACTION SUMMARY ` centred
  (30 + 20 + 30), 80 `-`, column titles (`Tran ID` X(16), `Tran Details` X(51), `  Tran Amount` X(13)), 80 `-`, the
  transaction lines, 80 `-`, the total, `ST-LINE15` = 18 + n lines. Baseline: 50 statements, 312 transactions, 1262 lines.
- `HTMLFILE` 100 bytes: 39 lines before the transactions + 15 per transaction + 8 after = 47 + 15n per statement;
  baseline 6632 lines.
- Text is compared byte for byte (the equivalence also normalises trailing spaces, as the ticket asks); HTML is compared
  after whitespace normalisation and is byte-identical as well.

## Java mapping

`Cbstm03a.run()` keeps the COBOL sequence: `loadTransactions()` (R-3) fills `TransactionTable`, a byte array laid out
like `WS-TRNX-TABLE` (offsets `(cr-1)*3356 + 16 + (tr-1)*334`) plus the `WS-TRCT` counts, so R-4's overlay is the same
memory effect rather than a simulation; the main loop (R-5/R-6), `createStatement()` (R-7/R-8/R-12),
`writeTransactions()` (R-9/R-10) and `writeTransaction()` (R-11). `string(length, source, delimiter, …)` is COBOL
`STRING … DELIMITED BY` into a space-filled target; `NumericEdited` formats `9(9).99-` / `Z(9).99-`; `TRNX-AMT` of a
garbled (overlaid) entry is read like GnuCOBOL reads invalid zoned digits (low nibble, `zoned()`). All file access is
`Cbstm03b.call(dd, operation[, key, keyLength])`. Output goes to two `BufferedSink`s written as dated generations when
the step ends normally.

## Verification

- `CreastmtBaselineTest`: STEP010 sort + OUTREC of the COMBTRAN after-image = `TRXFL.SEQ.txt`; STEP020 key checks;
  CBSTM03A over the baseline TRXFL / CARDXREF / CUSTDATA / INTCALC ACCTDATA after-images = `STMTFILE.txt` and
  `HTMLFILE.txt`, byte for byte.
- `Cbstm03aEdgeCaseTest` vs GnuCOBOL output for the same synthetic inputs: cards without transactions (R-9), a card with
  12 transactions (R-4 overlay), empty TRXFL (R-3 abend); and the Java-only table-overflow abend (R-4).
- `Cbstm03aHtmlEscapeTest`: D-1 in both modes.
- `CreastmtJobIT` (Testcontainers PostgreSQL): the `creastmt` stream in table mode from `initial-load` + `repro` of the
  baseline after-images, file mode, the empty-TRXFL abend (no statement generation) and the bypass of later steps.
- `scripts/batch/run_creastmt.sh file|table` + `compare_creastmt.py` (CI job `batch-equivalence`).
