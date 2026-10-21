# carddemo-batch-posttran

Java port of the `POSTTRAN` batch job (`app/jcl/POSTTRAN.jcl`, program `app/cbl/CBTRN02C.cbl`):
Spring Boot 4.1 / Spring Batch 6 / JPA on Java 25. The four VSAM KSDS files become tables
(H2 by default, PostgreSQL via the `postgres` profile); DALYTRAN and DALYREJS stay flat files.

Paragraph-by-paragraph mapping: [docs/CBTRN02C-mapping.md](docs/CBTRN02C-mapping.md).

## Build and test

```bash
cd java/posttran
./mvnw verify        # needs JDK 25
```

`GoldenMasterTest` runs the job against both seed sets (`app/data/ASCII`, `app/data/EBCDIC`) and
compares SYSOUT, return code, DALYREJS, TRANSACT, ACCTDATA and TCATBAL record-for-record with the
output of the original COBOL (`src/test/resources/golden/{ascii,ebcdic}`).

## Run

```bash
java -jar target/carddemo-batch-posttran-*.jar \
  --posttran.seed=EBCDIC \
  --posttran.dalyrejs=target/dalyrejs.dat \
  --posttran.export-dir=target/out
echo $?   # 4 when any transaction was rejected
```

| Property | Default | Meaning |
|---|---|---|
| `posttran.data-dir` | `../../app/data` | Seed data root |
| `posttran.seed` | `ASCII` | `ASCII` or `EBCDIC`: seed dataset; also picks the default DALYTRAN file and format |
| `posttran.load-seed` | `true` | Reload ACCOUNT, CARD_XREF, TRAN_CAT_BALANCE from the seed (and empty TRANSACT) before the run; set `false` to keep existing tables |
| `posttran.dalytran` | seed's daily file | DALYTRAN input |
| `posttran.dalyrejs` | `target/dalyrejs.txt` | DALYREJS output (430-byte records) |
| `posttran.format` | seed's format | `ASCII` (ISO-8859-1, one record per line) or `EBCDIC` (CP037, fixed length, no delimiters) |
| `posttran.export-dir` | none | Unload TRANSACT/ACCTDATA/TCATBAL here after the run, in key order |
| `posttran.chunk-size` | `1` | Commit interval |

Exit codes: `0` all posted, `4` one or more rejects, `8` posted but `export-dir` unload failed, `12` abend (the equivalent of
`9999-ABEND-PROGRAM`; the job ends `FAILED`, the abend reason is on SYSOUT).

PostgreSQL: `--spring.profiles.active=postgres` with `POSTTRAN_DB_URL` (optional), `POSTTRAN_DB_USER`,
`POSTTRAN_DB_PASSWORD`.

## Regenerating the golden master

Requires GnuCOBOL 3.1+ (`cobc`) and Python 3.

```bash
tools/gnucobol/run-cobol-posttran.sh ascii
tools/gnucobol/run-cobol-posttran.sh ebcdic
```

The script compiles the unmodified `CBTRN02C.cbl` with the repo copybooks, loads the seed data into
GnuCOBOL indexed files, runs it, and unloads every output. `TRAN-PROC-TS` (columns 305-330) is the
wall clock and is masked with `#`; the Java test masks the same columns.
