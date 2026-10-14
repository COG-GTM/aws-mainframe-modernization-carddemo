# CardDemo - Java migration

Java 25 / Spring Boot 4.1 port of the CardDemo COBOL application, one program per PR. The COBOL sources
under `../app` stay the single source of truth and are never modified; BMS maps and seed datasets are read
from there at build time.

## Layout

| Module | Contents |
|---|---|
| `pom.xml` | Parent (`com.carddemo:carddemo-parent`), Spring Boot 4.1.1 parent, Java 25. Property `carddemo.app.dir` points at `../app`. |
| `carddemo-runtime` | Shared emulation layer used by every program (no Spring dependency). |
| `carddemo-online` | CICS online programs as Spring services + one generic REST adapter. |
| `carddemo-batch` | *Not created yet* - the first batch session adds it to `<modules>` (Spring Batch). |
| `carddemo-vsam-mq`, `carddemo-db2`, `carddemo-ims-db2-mq` | *Not created yet* - created by the first tier-4 session that needs each. |
| `docs/mapping/<PROGRAM>.md` | Per-program mapping doc (paragraph -> method, fields, expected-value derivation, LIMITATIONS). |

### carddemo-runtime packages

| Package | What it emulates |
|---|---|
| `com.carddemo.runtime.cobol` | `PicX` (fixed-length PIC X: space pad / right truncate, SPACES/LOW-VALUES/HIGH-VALUES, UPPER-CASE), `CobolNumeric` (MOVE/COMPUTE store: truncation, `ROUNDED` = half-up), `ZonedDecimal` (PIC 9 DISPLAY, overpunch sign), `PackedDecimal` (COMP-3), `BinaryInteger` (COMP), `CobolRecord` + `RecordLayout` + `AlphanumericField`/`NumericField` (copybook record codecs, groups, FILLER, REDEFINES), `Condition88` / `CobolCode` (88-levels), `Ebcdic` (CP037), `CobolIntrinsics.currentDate`. |
| `com.carddemo.runtime.cics` | `CicsProgram` (a migrated program), `CicsContext` (EIB, commarea, RECEIVE/SEND MAP, SEND TEXT, ASSIGN, file control, RETURN / RETURN TRANSID / XCTL / ABEND), `CicsDispatcher`, `CicsResponse`, `CicsOutcome`, `AidKey`, `CicsResp` (RESP values), `InstalledPrograms` (PROGRAM resources for INQUIRE PROGRAM, from `app/csd/CARDDEMO.CSD` + `carddemo.cics.installed-programs`). |
| `com.carddemo.runtime.bms` | `BmsParser` (parses `app/bms/*.bms`), `BmsMapsetRegistry`, `SymbolicMap` (the `xxxxI/xxxxO` copybook: data, L, F/A, cursor), `BmsScreen` (what SEND MAP shows; 24x80 rendering), `Dfhbmsca` (DFHBMSCA colour bytes for `xxxC` overrides). |
| `com.carddemo.runtime.vsam` | `KsdsFile` (READ, READ UPDATE, GTEQ, WRITE, REWRITE, DELETE, STARTBR/READNEXT/READPREV/RESETBR/ENDBR, NOTFND/DUPREC/ENDFILE/LENGERR/INVREQ/DISABLED/NOTOPEN), `AlternateIndexPath`, `EsdsFile`, `RrdsFile`, `FileControl` (the FCT), stores `InMemoryKsdsStore` / `JdbcKsdsStore` (H2), `FlatFiles`. |
| `com.carddemo.runtime.batch` | `Job`, `JobStep`, `StepResult`, `DdBindings` (DD name -> path), `FileStatus`, `Abend`. |
| `com.carddemo.common.copybook` | Shared copybooks: `CardDemoCommarea` (COCOM01Y), `UserSecurityRecord` (CSUSR01Y), `ScreenTitles` (COTTL01Y), `CommonMessages` (CSMSG01Y), `DateTimeFields` (CSDAT01Y), `UserSecuritySeed`. |

## Build and test

```bash
cd java
./mvnw -B verify          # needs JDK 25 on PATH / JAVA_HOME
```

The wrapper downloads Maven 3.9.11. If Maven Central is unreachable/rate-limited, point the wrapper at a
mirror with `MVNW_REPOURL=<mirror>/maven2` (and a matching mirror in `~/.m2/settings.xml`).

Run the online region (H2 in-memory, USRSEC seeded from `app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS`):

```bash
./mvnw -B -pl carddemo-online -am install -DskipTests
java -jar carddemo-online/target/carddemo-online-0.1.0-SNAPSHOT.jar
curl -s -XPOST localhost:8080/api/cics/CC00 -H 'Content-Type: application/json' -d '{}'
```

### REST contract (`carddemo-online`)

`POST /api/cics/{transid}` runs one task of the program bound to the transaction;
`POST /api/cics/programs/{program}` runs a program directly (what an XCTL would do);
`GET /api/cics/transactions` lists bindings. Request:

```json
{ "aid": "ENTER", "commarea": "<base64, omit for EIBCALEN=0>", "fields": { "USERID": "admin001", "PASSWD": "password" } }
```

Response: `outcome` (`RETURN`, `RETURN_TRANSID`, `XCTL`, `ABEND`), `nextTransid`, `nextProgram`,
`commarea` (base64) + `commareaFields` (decoded with the program's copybook), `screen` (fields, values,
cursor), `screenLines` (24x80), `text` (SEND TEXT), `eibresp`/`eibresp2`, `abendCode`. A client continues the
pseudo-conversation by posting the returned commarea to `nextTransid`, or to `programs/{nextProgram}` after an XCTL.

## Contract for later sessions

1. **One program per package**: `com.carddemo.online.<program>` / `com.carddemo.batch.<program>`. Online
   programs implement `CicsProgram`, are Spring `@Component`s (the REST adapter picks them up), and keep each
   COBOL paragraph as a method of the same name with a `(lines a-b)` reference.
2. **Stateless programs**: create WORKING-STORAGE per task (see `Cosgn00c.Task`). Fields without a `VALUE`
   start as LOW-VALUES (`new CobolRecord(len)`, `SymbolicMap.of(...)`).
3. **Control flow**: `ctx.xctl`, `ctx.returnTransid`, `ctx.returnToCics`, `ctx.abend` never return (they end
   the task like CICS). Never call a not-yet-migrated program: XCTL just reports `nextProgram`.
4. **Copybooks**: shared copybooks live in `carddemo-runtime` `com.carddemo.common.copybook` as
   `CobolRecord` subclasses with a static `RecordLayout`; reuse before adding. Program-private copybooks stay
   in the program package.
5. **Files**: register each CSD `DEFINE FILE` in `OnlineRegionConfig.fileControl` (`KsdsFile` over a
   `JdbcKsdsStore`), seeded from `app/data` when empty. Copy the dataset into resources via the runtime pom
   (as done for USRSEC) rather than duplicating data.
6. **Runtime changes** must be generic and backward compatible; all existing tests stay green.
7. **Tests + docs**: JUnit 5 parity tests per paragraph with expected values traced to COBOL line numbers;
   `docs/mapping/<PROGRAM>.md` with paragraph table, field mapping, derivations and LIMITATIONS.
   Batch programs also get GnuCOBOL golden files (`carddemo-batch/tools/gnucobol/`).
