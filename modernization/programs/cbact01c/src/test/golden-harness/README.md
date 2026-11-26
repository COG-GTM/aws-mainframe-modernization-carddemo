# GnuCOBOL golden outputs for CBACT01C

`src/test/resources/golden/` holds the outputs of the original `app/cbl/CBACT01C.cbl` run by GnuCOBOL 3.1.2
over `app/data/ASCII/acctdata.txt` (copied to `src/test/resources/fixtures/`). `GnuCobolGoldenTest`
compares the Java port with them.

Regenerate from the repository root:

```sh
modernization/programs/cbact01c/src/test/golden-harness/generate.sh
```

Harness-only changes (the program logic is untouched):

- `ACCTFILE` is switched from `ORGANIZATION IS INDEXED` to `LINE SEQUENTIAL` so the ASCII unload can be read
  without building a BDB KSDS (the unload is already in key order).
- `COBDATFT.cbl` is a COBOL stand-in for the assembler routine `app/asm/COBDATFT.asm`.
- `CEE3ABD.cbl` stands in for the LE abend service (`STOP RUN RETURNING 999`).
- Compiled with `-std=ibm -fsign=EBCDIC` so zoned signs use the `{`/`A-I`/`}`/`J-R` overpunch of the data.
