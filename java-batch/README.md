# CardDemo Java Batch (demo sample code)

Spring Batch 5 / Spring Boot 3 / Java 17 port of the CardDemo COBOL batch layer. Reads and writes the ASCII
sample files in `../app/data/ASCII`; `app/` itself is never modified.

```
JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64 mvn -B verify
```

- `com.carddemo.batch.model`: one record per copybook (CVACT01Y, CVACT03Y, CVTRA01Y, CVTRA02Y, CVTRA05Y,
  CVTRA06Y, CUSTREC, COSTM01), each with a byte-exact `CODEC`. Money is `BigDecimal` at the copybook scale.
- `com.carddemo.batch.io`: fixed-width field reader/writer, zoned-decimal overpunch, Spring Batch
  reader/writer factories (`FixedWidthItemIO`), the in-memory VSAM KSDS stand-in (`KsdsFile`) and
  `CardDemoData` for locating the sample files (`-Dcarddemo.data.dir` overrides).
- `com.carddemo.batch.jobs`: one sub-package per COBOL program (`posttran`, `intcalc`, `creastmt`).

Data note: in `acctdata.txt` the value `A000000000` sits in `ACCT-ADDR-ZIP` and `ACCT-GROUP-ID` is blank.
