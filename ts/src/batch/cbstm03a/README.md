# CBSTM03A — account statements (CREASTMT STEP040)

Port of `app/cbl/CBSTM03A.CBL`. All file I/O goes through
[CBSTM03B](../cbstm03b/README.md) via the `M03bArea` CALL interface.

```ts
const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
buildTrnxFile(catalog);   // CREASTMT STEP010 SORT/OUTREC + STEP020 REPRO -> TRXFL KSDS (alias TRNXFILE)
const { stmtRecords, htmlRecords, display } = runCbstm03a({ catalog, stmtFile, htmlFile });
```

- `trnx-file.ts`: TRNXFILE build. Sorts TRANSACT on card (263,16) + tran id
  (1,16) in EBCDIC order, reformats to COSTM01 (card first), pads to 350.
  There is no TRANSACT seed: use `transactFromDailytran` or POSTTRAN output.
- `index.ts`: the program. Paragraphs are a label state machine; `ALTER
  8100-FILE-OPEN TO PROCEED TO ...` is the `alter8100` variable, so the open
  sequence TRNXFILE (+ load table) -> XREFFILE -> CUSTFILE -> ACCTFILE ->
  1000-MAINLINE follows the same GO TO chain as the COBOL.
- WS-TRNX-TABLE is a byte buffer (51 x (16 + 10 x 334)) with COBOL
  addressing and no subscript checking: an 11th transaction for a card
  overlays the next card slot exactly as on z/OS; going past the whole table
  (52nd card) throws `StorageOverlayError` instead of corrupting memory.
- `cobol-string.ts`: `STRING ... DELIMITED BY` (name / address / HTML lines).
- `html.ts`: the HTML-FIXED-LN 88-level literals.
- Total: `ADD TRNX-AMT TO WS-TOTAL-AMT` on S9(9)V99 with Decimal and
  high-order truncation (no ON SIZE ERROR). Edited with `Z(9).99-`.
- Errors: `Cbstm03aAbend` (CEE3ABD) carrying the SYSOUT lines and the records
  produced so far; output files are only written at normal end
  (DISP=(NEW,CATLG,DELETE)).

## Testing
`test/batch/cbstm03a/golden.test.ts` compares all 1250 STMTFILE and 6500
HTMLFILE lines (right-trimmed) against output of the real COBOL under GnuCOBOL
3.1.2, regenerated with `tools/gen-golden-cbstm03a.sh` (needs `gnucobol3`).
Unit tests cover CALL order, abends, totals, negative amounts, OCCURS overflow.

## LIMITATIONS
- **PSA/TCB/TIOT walk**: the COBOL reads z/OS control blocks to DISPLAY the job
  name, step name and DD names. Here they come from `JobContext`
  (`CREASTMT_STEP040` by default, `job: null` to skip); UCB presence is
  caller-supplied, not discovered. The quirk that the last TIOT entry is
  printed after the loop with `-- null  UCB` (two spaces) text is kept. This
  block is removed from the GnuCOBOL golden build (it cannot run off z/OS), so
  the DISPLAY lines are traced from the source, not golden-tested.
- **Golden build tweaks**: `CUSTREC.cpy` is tab-indented, which `cobc` rejects
  in fixed format; the golden script re-indents a temporary copy. GnuCOBOL
  indexed files stand in for the VSAM KSDSs.
- Alphanumeric comparisons in 4000-TRNXFILE-GET and the SORT use the EBCDIC
  collating sequence (z/OS), while GnuCOBOL uses ASCII; identical for the
  all-digit card numbers in the seed data.
- **HTML injection (bug-for-bug)**: the COBOL puts customer, account and
  transaction text into HTMLFILE without escaping, so data that contains markup
  is rendered (or executed) by whoever opens the statement. That is still the
  default here. `runCbstm03a({ ..., escapeHtml: true })` HTML-escapes every data
  value (`& < > " '`) while leaving the fixed markup and STMTFILE unchanged.
  Escaped lines may be cut off at 100 bytes. On the seed data the only change is
  `'` becoming `&#39;` in a few merchant and customer names. Turn it on for any
  output a browser will display.
- Output files are written as LINE SEQUENTIAL (one record per line, optional
  trailing-space trim) instead of FB 80 / FB 100 datasets.
