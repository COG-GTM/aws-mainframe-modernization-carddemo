# CBEXPORT (and CBIMPORT) — batch export / import

TypeScript port of `app/cbl/CBEXPORT.cbl` (this directory) and
`app/cbl/CBIMPORT.cbl` (`../cbimport/`). Both use the 500-byte CVEXPORT
multi-record layout (`ExportRecordLayout` in `src/common/copybooks/export.ts`):
record types `C` customer, `A` account, `X` card xref, `T` transaction,
`D` card, with real COMP and COMP-3 fields.

```ts
import { defineExportCluster, runCbexport } from './batch/cbexport/index.js';
import { runCbimport } from './batch/cbimport/index.js';

const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
defineExportCluster(catalog);                       // JCL DELETE/DEFINE CLUSTER step
runCbexport({ catalog });                           // CUSTFILE..CARDFILE -> EXPFILE
runCbimport({ catalog, dd: { CUSTOUT: 'c.dat', ACCTOUT: 'a.dat', XREFOUT: 'x.dat',
  TRNXOUT: 't.dat', CARDOUT: 'd.dat', ERROUT: 'e.dat' } });
```

- `runCbexport` reads the five KSDS inputs sequentially (key order) by DD name
  and writes customers, accounts, xrefs, transactions, cards in that order to
  `EXPFILE`, numbering records 1..n (`EXPORT-SEQUENCE-NUM`, COMP), with branch
  `0001`, region `NORTH` and the `'YYYY-MM-DD HH:MM:SS.00'` timestamp.
- `runCbimport` reads `EXPFILE` in key order and writes each type to its FIXED
  sequential file (500/300/50/350/150 bytes); other types go to `ERROUT`
  (132 bytes: CURRENT-DATE, type, ERR-SEQUENCE 9(7), message) and are counted.
  Its "validation" step only DISPLAYs that no errors were detected, as in COBOL.
- `charset` (default `'ascii'`, the catalog convention) selects the encoding of
  display fields in the export file and the import outputs; `'ebcdic'` gives
  the z/OS bytes (COMP/COMP-3 are encoding-independent).
- Both return `{ abended, displays, stats }`; `9999-ABEND-PROGRAM`
  (`CALL 'CEE3ABD'`) becomes `abended: true` after the same DISPLAY lines.

## Testing

`test/batch/cbexport/golden/gen-golden.sh` runs the real programs under
GnuCOBOL 3.1 (indexed files for VSAM): CBEXPORT on the ASCII seeds, then
CBIMPORT on that export plus two unknown-type records. `golden.test.ts`
requires byte-identical export records (timestamp masked), byte-identical
import files and ERROUT records (CURRENT-DATE masked) and the same DISPLAY
lines. Unit tests also cover the reference EBCDIC export
(`app/data/EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS`), round trips and abends.

## LIMITATIONS

- **RECORD KEY in WORKING-STORAGE.** Both programs declare
  `RECORD KEY IS EXPORT-SEQUENCE-NUM`, a WORKING-STORAGE item; neither
  Enterprise COBOL nor GnuCOBOL compiles that. The port uses the cluster
  definition from CBEXPORT.jcl, `KEYS(4 28)`: bytes 28-31, i.e. the low three
  bytes of the sequence number plus the first byte of EXPORT-BRANCH-ID (the
  field itself is at offset 27). Order equals sequence order below 2**24
  records. The golden script compiles copies whose only change is an FD key
  field over the same bytes.
- **Import FILLER.** `INITIALIZE` skips FILLER, so the output records' FILLER
  is whatever the FD record area held; the port writes LOW-VALUES, which is
  what GnuCOBOL produces (on z/OS it depends on the QSAM buffer).
- **Missing DD.** CBIMPORT.jcl has no `CARDOUT` DD. A DD omitted from `dd`
  fails its OPEN with status 35 and abends (CBLQDA(OFF) behaviour assumed).
- **Reference export file.** It matches `runCbexport` on the EBCDIC seeds
  except: customers 1/50 and accounts 1/50 hold later (online-edited) data;
  sequence numbers 451-459 are skipped (cards are 460-509); timestamps carry
  six fractional digits (`.000000`), whereas CBEXPORT.cbl writes `.00`.
- **Clock.** ACCEPT DATE/TIME and CURRENT-DATE use the `now` option (local
  time); CURRENT-DATE's UTC offset comes from the JS timezone.
- Numeric MOVEs go through decode/encode (decimal.js), so invalid numeric data
  in an input field is normalised rather than copied byte for byte.
