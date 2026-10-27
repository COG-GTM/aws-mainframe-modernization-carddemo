# Java 21 modernization scaffold

Plain Java immutable copybook records and fixed-width codecs live in records.
interest, posting, and batch are empty port destinations. The parent imports
the Spring Boot 3.5 BOM for dependency versions; no Spring runtime is required.

Build from the repository root with JDK 21 and Maven:

    mvn -f modernization/java21/pom.xml verify

## Codec conventions

Records retain all fields including filler in copybook order. Component Javadoc
cites source PIC and line. Numeric identifiers remain fixed-width digit strings.
Text decoding retains spaces; encoding space-pads short text and rejects overflow
and unmappable characters. Record decoders require the exact copybook byte length.

Each record exposes CODEC, with ASCII defaults and explicit charset overloads.
TextCodec.EBCDIC is IBM037. Signed DISPLAY uses trailing {ABCDEFGHI / }JKLMNOPQR
overpunch (C/D zones in IBM037). Money and rates use ZonedDecimal.value(), a
BigDecimal at the exact PIC scale. The immutable wrapper retains a negative-zero
flag because BigDecimal alone cannot preserve it. Wrong scales and overflow are
rejected without rounding. Unsigned PIC 9 fields accept digits only.

Standalone COMP-3 uses C/D signs, accepts F on decode, and canonicalizes positive
signs to C on encode. Standalone signed COMP is big-endian, with 2/4/8 bytes for
1-4/5-9/10-18 digits. The core money fields in CVACT01Y, CVTRA01Y, CVTRA02Y,
CVTRA05Y and CVTRA06Y are zoned DISPLAY S9(n)V99, **not COMP-3**.
COMP-3 appears in CVEXPORT and working storage instead.

## Shipped fixture findings

* All 50 cardxref.txt lines are 36 bytes: exactly the three non-filler fields.
  The 14-byte filler is absent in this text export; this observation does not
  establish how the source KSDS was unloaded. The codec implements the 50-byte
  copybook. Tests append 14 spaces and compare full padded bytes.
* discgrp.txt contains 51 nonblank 50-byte records, not a blank extra line.
  The last starts with ZEROAPR followed by three spaces and 070001.
* Six dailytran.txt amount fields end in a closing brace, but their magnitudes
  are nonzero: negative amounts ending in digit zero. No negative-zero money
  field occurs in the supplied fixtures. Synthetic primitive and record tests
  verify byte-exact negative-zero preservation.
* CBTRN02C.cbl:176-182 defines the 430-byte reject record and trailer:
  four-digit reason plus 76-character description. Lines 446-448 copy the
  transaction and working-storage trailer. RejectRecord retains transaction
  data as opaque X(350), since rejected numeric fields may be invalid.

Tests read every ASCII fixture line from the repository, round-trip each layout
in ASCII and IBM037, and exercise standalone numeric codecs.
