# CardDemo on Java 21 (Phase 0 foundation)

Maven multi-module target platform for the COBOL → Java 21 migration of CardDemo: Spring Boot 3.5, Spring Batch,
Spring Data JPA, MapStruct, Flyway, Testcontainers. Phase 0 contains the build, the data model and the
golden-master test harness; business logic is migrated by later workstreams (see [MIGRATION-MAP.md](MIGRATION-MAP.md)).

| Module | Contents |
|:--|:--|
| `carddemo-common` | COBOL data types: `PackedDecimal` (COMP-3), `ZonedDecimal` (DISPLAY, overpunch), `CobolBinary` (COMP/COMP-5), `Ebcdic`, `PicClause`, `NumericEdited`; `CopybookParser` → `RecordLayout`/`RecordView` (REDEFINES, OCCURS, usage inheritance); `RecordFileReader/Writer` (FB, VB/RDW, text); annotation-driven `RecordCodec`. Test-jar: golden-master harness. |
| `carddemo-data` | JPA entities + repositories for the core VSAM files and the optional IMS pending-authorization segments, Flyway schema, copybook records + MapStruct mappers, `CardDemoDataImporter` (REPRO equivalent). Copybooks are packaged from `app/cpy` at build time. |
| `carddemo-online` | Spring Boot web skeleton (CICS replacement). |
| `carddemo-batch` | Spring Batch skeleton (JCL replacement) + `CopybookRecordItemReader/Writer`. |

## Build

```bash
cd java
JAVA_HOME=/path/to/jdk-21 mvn -B verify          # all tests; Testcontainers tests need Docker (skipped without it)
mvn -B verify -Dgroups=golden-master             # parity tests only
mvn -B test -pl carddemo-common -Dgolden.update=true   # re-approve golden snapshots after a reviewed change
```

## Golden-master harness (`carddemo-common` test-jar)

Add `carddemo-common` with `<type>test-jar</type>` (test scope) and annotate the test class with `@GoldenMasterTest`;
`MainframeSources` and `GoldenMaster` are injected.

```java
@GoldenMasterTest
class PostTransactionsParityTest {
    @Test
    void matchesMainframeOutput(MainframeSources sources, GoldenMaster golden) {
        RecordLayout layout = sources.layout("CVTRA05Y");
        List<byte[]> expected = RecordFileReader.readAll(sources.resolve("expected/TRANSACT.dat"),
                RecordFormat.FIXED, layout.length(), DataEncoding.EBCDIC);
        List<byte[]> actual = runJavaPosting();
        golden.assertMatches("POSTTRAN", layout, expected, DataEncoding.EBCDIC, actual, DataEncoding.EBCDIC);
    }
}
```

* Comparison is record by record, field by field, on decoded values (so EBCDIC and ASCII outputs compare
  directly); failures list `record n FIELD @offset: expected [...] but was [...]`.
* Accepted divergences are declared as `KnownDifference(record, field, reason)`; the test fails if a declared
  difference stops occurring.
* `assertMatchesSnapshot(name, lines)` approves text output (reports, layouts) against
  `src/test/resources/golden/<name>.golden`.
* `CardDemoDataset` enumerates the sample datasets in `app/data` with their copybook and LRECL.

For database-backed tests, `carddemo-data`'s test-jar provides `@CardDemoDataTest` and
`PostgresTestcontainersConfiguration` (PostgreSQL 16, Flyway applied, Hibernate `validate`).
