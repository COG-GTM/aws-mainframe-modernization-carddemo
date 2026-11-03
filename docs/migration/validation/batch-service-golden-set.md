# batch-service golden set (UNT149-15)

The Java jobs CREASTMT, CBEXPORT, CBIMPORT, READACCT, READCARD, READCUST and READXREF compared byte for
byte with the unmodified COBOL run under GnuCOBOL on the same input.

## How the reference is produced

`scripts/migration/golden/batch-service-reference.sh` compiles CBSTM03A/B, CBEXPORT, CBIMPORT,
CBACT01C, CBACT02C, CBACT03C and CBCUS01C with `cobc` (temporary copies; `app/` is not modified),
loads the `app/data/ASCII` files into indexed files, runs each program under `faketime
'2022-07-18 10:30:45'` (UTC) and writes every output DD, SYSOUT and the return code to
`modernization/batch-service/src/test/resources/golden/<SET>/expected/`. Re-running it gives the
same 42 files (checked with `sha256sum` before/after).

Compile shims (as in [scope.md](../scope.md) §5): CBSTM03A's tabbed `CUSTREC.cpy` expanded and
its TIOT walk (DISPLAY only, z/OS control blocks) removed; the CBEXPORT/CBIMPORT FD key field placed
at offset 27 for `RECORD KEY IS EXPORT-SEQUENCE-NUM`; the Assembler `COBDATFT` re-expressed in
COBOL so CBACT01C can run. The shims are reference-only; the Java code replaces `COBDATFT` with
`java.time`.

| Set | COBOL | Input | Compared |
|---|---|---|---|
| CREASTMT | SORT (STEP010) + CBSTM03A/B | `dailytran.txt` as TRANSACT, `cardxref`, `custdata`, `acctdata` | `TRXFL.SEQ`, `STMTFILE`, `HTMLFILE`, SYSOUT, RC |
| CBEXPORT | CBEXPORT | the five sample files | `EXPFILE` (500-byte CVEXPORT, binary COMP fields), SYSOUT, RC |
| CBIMPORT | CBIMPORT | the reference `EXPFILE` | `CUSTOUT`, `ACCTOUT`, `XREFOUT`, `TRNXOUT`, `CARDOUT`, `ERROUT`, SYSOUT, RC |
| CBIMPORT-EDGE | CBIMPORT | `EXPFILE` with unknown record types | as above; `ERROUT` has 132-byte error records |
| READACCT | CBACT01C | `acctdata.txt` | `OUTFILE`, `ARRYFILE`, `VBRCFILE` (variable), SYSOUT, RC |
| READACCT-EDGE | CBACT01C | accounts with non-zero ACCT-CURR-CYC-DEBIT (the sample data has only zeros, which take the `2525.00` default) | as above |
| READCARD / READCUST / READXREF | CBACT02C / CBCUS01C / CBACT03C | the sample file | SYSOUT, RC |

`VBRCFILE`: Java writes the z/OS RDW (length including the 4-byte RDW); GnuCOBOL writes its own
record-length prefix. The test converts the Java file to the GnuCOBOL form and compares bytes.

## Results

`GoldenSetTest` (9 sets): all outputs and return codes identical.

`BranchRoundTripTest` (PostgreSQL via Testcontainers, the four Flyway-created schemas that hold
the exported data, rows inserted from the sample files; `transaction.transactions` from
`dailytran.txt` as if posted):

- CBEXPORT with no `--dd` reads the tables; CBIMPORT reads that `EXPFILE`. Each of CUSTOUT,
  ACCTOUT, XREFOUT, CARDOUT and TRNXOUT equals its table's unload, and the first four equal the
  sample files, everywhere except FILLER. ERROUT is empty.
- FILLER is binary zeros in every CBIMPORT output: the COBOL record areas start as low-values and
  `INITIALIZE` skips FILLER. The reference run shows the same bytes, so the Java job keeps them.
- CREASTMT reading the tables produces the same STMTFILE and HTMLFILE as the COBOL reference.

`AbendTest`: a duplicate KSDS key, a card with more than 10 transactions (CBSTM03A's table
`OCCURS 10`, an overflow the COBOL does not check) and CBIMPORT with no EXPFILE each end with RC 16.

## Accepted deviations

- HTMLFILE escapes `&`, `<` and `>` in customer name/address and transaction ID/description; CBSTM03A
  copies them as is, so stored data could inject markup. Quotes are left alone (element content only), and
  the sample data has none of the three, so the golden HTMLFILE bytes are unchanged. If escaping
  would push a line past the 100-byte record, the text is shortened by whole characters so the closing tag stays.
- Table-backed TRANSACT unload: `transaction.transactions` keeps no provenance and stores `orig_ts` as a
  timestamp, so a row gets the DB2 `YYYY-MM-DD-HH.MM.SS.NNNNNN` origin format only when it has the full
  record shape CBACT04C writes (type `01`, category `05`, source `System`, description `Int. for a/c …`);
  every other row gets the file format. An online add that copies all four would still be rendered as
  interest; telling them apart needs a provenance column in transaction-service.

## Not covered

- "Five schemas": `auth` has no CVEXPORT record type, so the round trip covers the five record types
  in the `customer`, `account`, `card` and `transaction` schemas.
- CBSTM03A beyond 51 cards / 10 transactions per card: COBOL behavior is undefined (storage
  overflow); Java abends instead. Not compared.
- EBCDIC: all sets are ASCII. `--codepage=ebcdic` uses the same layouts but is not compared here.
- `DateValidation` (CSUTLDTC) has unit tests only; there is no LE runtime to produce CEEDAYS reference
  results (see its Javadoc for the hypothesis).
