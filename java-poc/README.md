# CardDemo COBOL-to-Java proof of concept

A small, self-contained Java module that reproduces the behaviour of two CardDemo batch
programs, used to validate a migration approach before tackling the full application.

| COBOL program | JCL | Java entry point | Scope |
|---|---|---|---|
| `app/cbl/CBACT03C.cbl` | `app/jcl/READXREF.jcl` | `com.carddemo.poc.batch.Cbact03c` | Full program |
| `app/cbl/CBACT01C.cbl` | `app/jcl/READACCT.jcl` | `com.carddemo.poc.batch.Cbact01c` | Read + `DISPLAY` path only (see limitations) |

Both programs open a VSAM KSDS, read it sequentially until end of file, `DISPLAY` each record
and close the file. The Java output is byte-for-byte identical to the COBOL SYSOUT for the
sample data in `app/data`.

## Build and run

Requires JDK 21 and Maven 3.6+.

```bash
cd java-poc
mvn -B test                       # unit + integration tests against ../app/data
mvn -B package -DskipTests

# READXREF: //XREFFILE DD DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS
java -jar target/carddemo-batch-poc-0.1.0-SNAPSHOT.jar ../app/data/EBCDIC/AWS.M2.CARDDEMO.CARDXREF.PS

# READACCT: //ACCTFILE DD DSN=AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS
java -cp target/classes com.carddemo.poc.batch.Cbact01c ../app/data/EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS
```

Files ending in `.txt` are read as ASCII line-oriented text (`app/data/ASCII`); anything else
is read as EBCDIC (CP037) fixed-block (`app/data/EBCDIC`). The tests run both.

## What was migrated

```
java-poc/src/main/java/com/carddemo/poc
├── copybook/
│   ├── CardXrefRecord.java   COPY CVACT03Y  (50-byte card cross-reference record)
│   ├── AccountRecord.java    COPY CVACT01Y  (300-byte account record)
│   ├── ZonedDecimal.java     PIC 9(n) / PIC S9(n)V99 USAGE DISPLAY codec
│   └── PackedDecimal.java    PIC S9(n)V99 COMP-3 codec
├── io/
│   ├── FixedLengthRecordReader.java  SELECT ... ORGANIZATION INDEXED, ACCESS SEQUENTIAL
│   └── FileStatus.java               two-character FILE STATUS codes
└── batch/
    ├── BatchProgram.java     DISPLAY, 9910-DISPLAY-IO-STATUS, 9999-ABEND-PROGRAM
    ├── AbendException.java   CALL 'CEE3ABD'
    ├── Cbact03c.java         CBACT03C procedure division
    └── Cbact01c.java         CBACT01C procedure division (read/display path)
```

## COBOL construct to Java mapping

| COBOL | Java in this POC | Notes |
|---|---|---|
| `SELECT X ASSIGN TO DD ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL RECORD KEY ... FILE STATUS IS ...` | `FixedLengthRecordReader` | The DD name becomes a `Path` constructor argument (the JCL `DD` statement is replaced by a CLI argument). Sequential reads of a KSDS return records in key order; the sample `.PS` files are the REPRO source for the KSDS clusters and are already key-ordered, so a flat sequential read is behaviourally equivalent. |
| `FD` record (`FD-XREFFILE-REC`, `FD-ACCTFILE-REC`) | `byte[]` returned by `readNext()` | The FD area is only a landing buffer in these programs (`READ ... INTO` copies straight into WORKING-STORAGE), so it has no Java type of its own. |
| `COPY CVACT03Y` / `COPY CVACT01Y` (01-level group with 05-level `PIC` items) | `CardXrefRecord`, `AccountRecord` | Immutable classes wrapping the fixed-width record image. Each `PIC` item is a `(offset, length)` slice; `PIC X` fields are `String`, `PIC 9` keys are `long`, `PIC S9(10)V99` money fields are `BigDecimal` (scale 2). The raw image is kept so `DISPLAY <group>` can be reproduced exactly. |
| `PIC S9(10)V99` (zoned decimal, overpunched sign) | `ZonedDecimal.parse/format` | After CP037 -> ISO-8859-1 translation, the sign nibble surfaces as `{`/A-I (positive) and `}`/J-R (negative) in the last digit, e.g. `00000001940{` = `194.00`. Both sample encodings use this convention. |
| `PIC S9(10)V99 COMP-3` | `PackedDecimal.parse/format` | Not read by these programs but written by CBACT01C (`OUT-ACCT-CURR-CYC-DEBIT`, `ARR-ACCT-CURR-CYC-DEBIT`); included so the record-writing step has a tested codec. |
| `PIC 9(4) BINARY`, `PIC S9(9) COMP` | `int` | `TWO-BYTES-BINARY` / `REDEFINES` trick in `9910-DISPLAY-IO-STATUS` is replaced by `Character` arithmetic in `BatchProgram.displayIoStatus`. |
| WORKING-STORAGE items (`END-OF-FILE`, `APPL-RESULT`, `CARD-XREF-RECORD`) | private instance fields | Program state lives on the program object; each JCL step = one `new Cbact03c(...).run()`. |
| `88 APPL-AOK VALUE 0` / `88 APPL-EOF VALUE 16` | `APPL_AOK`, `APPL_EOF`, `APPL_ERROR` constants | Condition names become comparisons against constants. |
| `PROCEDURE DIVISION` main line | `run()` | |
| `PERFORM <paragraph>` | private method call | One method per paragraph, same name in camelCase, Javadoc carries the original paragraph name. |
| `PERFORM UNTIL END-OF-FILE = 'Y'` | `while (!endOfFile)` | |
| `DISPLAY ...` | `BatchProgram.display` -> `PrintStream.println` | The `PrintStream` is injected so tests capture SYSOUT. |
| `FILE STATUS` checks (`IF XREFFILE-STATUS = '00'`) | `FileStatus` enum returned/stored by the reader | The reader never throws; it sets a status the program interrogates, keeping the COBOL control flow intact. |
| `CALL 'CEE3ABD' USING ABCODE, TIMING` | `throw new AbendException(...)`; `main` exits 12 | ABCODE 999 exceeds the 0-255 process exit range; the exception exposes `abendCode() == 999` for callers/tests. |
| `GOBACK` | return from `run()` | |
| JCL `EXEC PGM=` + `DD` | `main(String[])` + `Path` argument | |

### Behavioural quirk preserved on purpose

`CBACT03C` displays every record **twice** — once inside `1000-XREFFILE-GET-NEXT` and again in
the main loop. `CBACT01C` likewise prints the field-by-field block from `1100-DISPLAY-ACCT-RECORD`
*and* the whole 300-byte `ACCOUNT-RECORD`. The Java port reproduces both so output can be
diffed against mainframe SYSOUT; removing the duplication is a one-line change once parity is
confirmed.

## Tests

`mvn test` runs 19 tests:

* `Cbact03cTest` – runs the program against the EBCDIC and ASCII cross-reference files and
  asserts the complete SYSOUT (102 lines) against an independently derived expectation; checks
  first/last records; checks the abend path (`FILE STATUS IS: NNNN0035`, no `END OF EXECUTION`).
* `Cbact01cTest` – same for the account file (652 lines). Note the ASCII and EBCDIC sample
  files differ in record 49 (`ACCT-GROUP-ID` = `ZEROAPR` vs blank), so each is compared to its own
  source.
* `FixedLengthRecordReaderTest` – EBCDIC and ASCII sources decode to identical records; file-status
  semantics for missing/unopened files.
* `ZonedDecimalTest`, `PackedDecimalTest`, `CardXrefRecordTest`, `AccountRecordTest` – codec and
  layout tests.

The tests read the sample data directly from `../app/data`; override with
`-Dcarddemo.data.dir=<path>`.

## Known limitations

* **CBACT01C output files are not written.** Paragraphs `1300`–`1575` populate and write
  `OUTFILE` (LRECL 107, mixed DISPLAY/COMP-3), `ARRYFILE` (LRECL 110, `OCCURS 5`) and `VBRCFILE`
  (RECFM VB, `RECORD IS VARYING ... DEPENDING ON`). The codecs needed (`ZonedDecimal.format`,
  `PackedDecimal.format`) are in place and tested; the writer and the `VBRC-REC1/REC2` DISPLAYs
  are the next increment.
* **`CALL 'COBDATFT'` (assembler, `app/asm`) is not ported.** It reformats the reissue date for
  `OUT-ACCT-REISSUE-DATE`; the Java equivalent is a `java.time` formatter once the assembler
  contract (`CODATECN` copybook) is confirmed.
* **No real KSDS.** Random/keyed access (`READ ... KEY IS`, `START`, alternate indexes used by
  other programs such as CBACT04C) is not modelled; the reader only supports the sequential
  browse these two programs use.
* **No Spring Batch.** The programs are ~150 lines with a single reader and no writer/commit
  logic, so a framework would obscure the 1:1 mapping. See next steps for where it fits.
* Fixed code page CP037; other CardDemo installations may use CP1047 or CP500.
* `DISPLAY` of numeric items prints the raw zoned picture (`00000001940{`) exactly as Enterprise
  COBOL does. If the target is a human-readable report rather than SYSOUT parity, switch to
  `AccountRecord.getCurrBal()` (`BigDecimal`) and format explicitly.

## Recommended next steps

1. **Finish CBACT01C** – add a `FixedLengthRecordWriter` (FB and VB), port paragraphs 1300–1575,
   and compare output data sets byte-for-byte with `AWS.M2.CARDDEMO.ACCTDATA.PSCOMP/ARRYPS/VBPS`
   produced on the mainframe.
2. **Generate copybook classes instead of hand-writing them.** `CVACT01Y`/`CVACT03Y` were
   translated by hand; for the remaining ~30 copybooks (`CVCUS01Y`, `CVTRA05Y`, `CVCRD01Y`, ...)
   use a copybook parser (e.g. cb2xml / JRecord, or a small ANTLR grammar) to emit the
   `(offset, length, picture)` tables and `BigDecimal`/`String`/`LocalDate` accessors.
3. **Batch orchestration (JCL -> Spring Batch).** Each JCL job (`POSTTRAN.jcl` -> `CBTRN02C`,
   `INTCALC.jcl` -> `CBACT04C`, `TRANREPT.jcl` -> `CBTRN03C`, ...) maps to a Spring Batch `Job`;
   each `EXEC PGM=` step to a `Step` with `ItemReader` (this POC's reader) / `ItemProcessor`
   (paragraph logic) / `ItemWriter`. `DD` statements become job parameters, `COND=` / `IF-THEN`
   become step flow decisions, GDGs become dated file names, and the Control-M definitions under
   `app/scheduler` map to the scheduler that triggers the jobs (Airflow, AWS Step Functions, or
   Control-M itself).
4. **Data stores.**
   * *VSAM KSDS/AIX* -> relational tables with the record key as primary key and each AIX as a
     unique/non-unique index (`CARDXREF` gains indexes on `XREF-ACCT-ID` and `XREF-CUST-ID`).
     Sequential browse = `ORDER BY key`; `START`/`READ KEY` = indexed lookup. Bulk-load with the
     existing REPRO sources via the `FixedLengthRecordReader` from this POC.
   * *Db2* – SQL is largely portable; translate `EXEC SQL` blocks to JDBC/JPA and host variables
     to parameters, keeping `SQLCODE` checks as exception mapping.
   * *IMS DB* – hierarchical segments become parent/child tables; `GU/GN/GNP` calls become
     queries along the foreign-key path.
5. **CICS online programs (`COxxxxC`) .** Each transaction (CC00, CA00, ...) becomes a stateless
   REST endpoint or a screen in a web UI: BMS maps (`app/bms`) define the request/response DTOs,
   `COMMAREA` (`COCOM01Y`) becomes a session/JWT-carried context object, `EXEC CICS READ/WRITE`
   becomes repository calls, `XCTL/LINK` becomes service calls, and pseudo-conversational
   `RETURN TRANSID` becomes the normal request/response cycle. Reuse the copybook classes from
   step 2 for every record the online programs touch.
6. **Parity harness.** Run the COBOL programs under GnuCOBOL (`cobc -std=ibm -I app/cpy`, already
   available in this repo's environment) or AWS M2 against the same data and diff SYSOUT/output
   data sets with the Java run in CI, program by program, before decommissioning each one.
