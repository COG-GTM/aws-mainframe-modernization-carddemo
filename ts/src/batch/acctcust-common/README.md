# Session 2 batch ports: CBACT01C, CBACT02C, CBACT03C, CBACT04C, CBCUS01C

| Program | Module | Entry point |
|---|---|---|
| CBACT01C (account file → OUTFILE/ARRYFILE/VBRCFILE, calls COBDATFT) | `../cbact01c/` | `runCbact01c(opts)` |
| CBACT02C (print CARDFILE) | `../cbact02c/` | `runCbact02c(opts)` |
| CBACT03C (print XREFFILE) | `../cbact03c/` | `runCbact03c(opts)` |
| CBACT04C (interest calculator) | `../cbact04c/` | `runCbact04c(opts)` |
| CBCUS01C (print CUSTFILE) | `../cbcus01c/` | `runCbcus01c(opts)` |

Each `run*` takes an optional `catalog` (default: a fresh `loadSeedCatalog()`), returns
`{ returnCode, abended, abendCode?, display, ... }` where `display` is SYSOUT, one entry per
DISPLAY, bytes as the COBOL writes them. 9999-ABEND-PROGRAM (CEE3ABD 999) gives
`abended: true, returnCode: undefined`. VSAM updates (CBACT04C REWRITE) are applied to the
catalog passed in. Output sequential files are written only when a path is given and are also
returned in memory (`outRecords`, `arryRecords`, `vbrcRecords`, `transactRecords`).

Shared here: `support.ts` (SYSOUT, 9910-DISPLAY-IO-STATUS, abend, `storeNumeric` truncating
numeric stores, `OutputFile`) and `ksds-print.ts` (the common CBACT02C/03C/CBCUS01C skeleton).

## Testing

- Unit tests next to each module (`*.test.ts`) on `app/data/ASCII` seed data.
- `test/golden-batch-acctcust.test.ts` compares SYSOUT, output files and the rewritten account file
  with the real COBOL run under GnuCOBOL 3.1 (`tools/gen-golden-batch.sh`; BDB indexed files stand
  in for VSAM). CBACT04C runs on the seed and on `test/golden/batch/fixtures` (built by
  `make-fixtures.py`: specific/DEFAULT/ZEROAPR groups, negative balances and rates,
  truncation and high-order overflow of WS-MONTHLY-INT).

## LIMITATIONS

- **DISPLAY of signed zoned items** follows Enterprise COBOL (raw digits with overpunched sign,
  e.g. `00000001940{`). GnuCOBOL prints `+0000000194.00`; the golden test converts these lines.
- **INITIALIZE of signed zoned items** (unused ARR-ACCT-BAL occurrences 4-5) stores a C-signed
  zero (`{`) as Enterprise COBOL does; GnuCOBOL stores `0`. Golden test normalises that byte.
- **COBDATFT** is a local TypeScript helper (`cbact01c/cobdatft.ts`); the golden run uses a COBOL
  stand-in (`test/golden/batch/cobol/COBDATFT.cbl`) because HLASM cannot run under GnuCOBOL. On the
  error path the assembler's `MVC COERMSG,=C'INVALID INPUT'` copies 38 bytes from the literal pool;
  the port pads with spaces instead of whatever follows the literal. CBACT01C never hits that path.
- **CBACT01C OUT-ACCT-CURR-CYC-DEBIT** is only moved when ACCT-CURR-CYC-DEBIT is zero, so other
  records keep the value of the previous record in the FD area (bug-for-bug). The FD area starts
  INITIALIZEd (zero) here; on z/OS its first content is undefined. CBACT01C never CLOSEs its three
  output files (GnuCOBOL closes them implicitly); the port closes them on return.
- **VBRCFILE** is written as RECFM=VB with IBM 4-byte RDWs (length includes the RDW). GnuCOBOL
  uses its own record header, so the golden test compares record data only.
- **CBACT04C last account is never rewritten**: the COBOL's `ELSE PERFORM 1050-UPDATE-ACCOUNT`
  belongs to the outer `IF END-OF-FILE = 'N'`, which is always true inside the loop. The interest
  transactions of the last account are written but its balance/cycle fields are not updated. Kept
  bug-for-bug (GnuCOBOL confirms).
- **CBACT04C XREF read by alternate key**: when an account has several cards VSAM returns status
  `02`, which the COBOL treats as an error (abend). The port follows VSAM; GnuCOBOL returns `00`
  there. Seed data has one card per account, so this does not occur on the seed.
- **CBACT04C timestamps**: TRAN-ORIG-TS/TRAN-PROC-TS come from `opts.now()` (local time,
  hundredths of a second); golden comparison masks them. PARM-DATE is padded/truncated to 10
  characters (a shorter EXEC PARM would leave undefined storage on z/OS).
- **WORKING-STORAGE without VALUE** (record areas, TRAN-RECORD) starts INITIALIZEd (spaces/zeros),
  as under GnuCOBOL; z/OS leaves it undefined. Only observable if e.g. TCATBALF is empty (then no
  1050 is performed anyway, see above).
- **TRANSACT** output is a plain fixed 350-byte sequential file (the JCL's SYSTRAN GDG); it is not
  loaded into the TRANSACT KSDS.
- **1400-COMPUTE-FEES** is empty in the COBOL and therefore a no-op here.
