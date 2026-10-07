package com.carddemo.common.codec;

import com.carddemo.common.file.RecordFiles;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Every sample dataset in app/data/EBCDIC decodes through its copybook, re-encodes byte for byte, and matches its
 * ASCII twin field by field except for the differences the GnuCOBOL baseline already measured.
 */
class SampleDataRoundTripTest {

    record Dataset(String id, String ebcdic, String copybook, String ascii, String baseline,
                   Function<FixedWidthRecord, String[]> variant) {

        Dataset(String id, String ebcdic, String copybook, String ascii, String baseline) {
            this(id, ebcdic, copybook, ascii, baseline, r -> new String[0]);
        }

        RecordLayout layout() {
            return Copybook.layout(copybook);
        }

        List<FixedWidthRecord> ebcdicRecords() {
            return RecordFiles.readFixed(id, TestData.resolve("app/data/EBCDIC/" + ebcdic), layout(),
                    RecordEncoding.EBCDIC);
        }

        List<FixedWidthRecord> asciiRecords() {
            return RecordFiles.readLines(id, TestData.resolve("app/data/ASCII/" + ascii), layout(),
                    RecordEncoding.ASCII);
        }

        @Override
        public String toString() {
            return id;
        }
    }

    private static String[] exportVariant(FixedWidthRecord r) {
        String variant = switch (r.getString("EXPORT-REC-TYPE")) {
            case "C" -> "EXPORT-CUSTOMER-DATA";
            case "A" -> "EXPORT-ACCOUNT-DATA";
            case "T" -> "EXPORT-TRANSACTION-DATA";
            case "X" -> "EXPORT-CARD-XREF-DATA";
            case "D" -> "EXPORT-CARD-DATA";
            default -> throw new AssertionError("unknown export record type in " + r);
        };
        return new String[] {variant, "EXPORT-TIMESTAMP-R"};
    }

    static final List<Dataset> DATASETS = List.of(
            new Dataset("ACCDATA", "AWS.M2.CARDDEMO.ACCDATA.PS", "CVACT01Y", "acctdata.txt", "ACCTDATA"),
            new Dataset("ACCTDATA", "AWS.M2.CARDDEMO.ACCTDATA.PS", "CVACT01Y", "acctdata.txt", "ACCTDATA"),
            new Dataset("CARDDATA", "AWS.M2.CARDDEMO.CARDDATA.PS", "CVACT02Y", "carddata.txt", "CARDDATA"),
            new Dataset("CARDXREF", "AWS.M2.CARDDEMO.CARDXREF.PS", "CVACT03Y", "cardxref.txt", "CARDXREF"),
            new Dataset("CUSTDATA", "AWS.M2.CARDDEMO.CUSTDATA.PS", "CVCUS01Y", "custdata.txt", "CUSTDATA"),
            new Dataset("DALYTRAN", "AWS.M2.CARDDEMO.DALYTRAN.PS", "CVTRA06Y", "dailytran.txt", "DALYTRAN"),
            new Dataset("DALYTRAN.INIT", "AWS.M2.CARDDEMO.DALYTRAN.PS.INIT", "CVTRA06Y", null, "DALYTRAN.INIT"),
            new Dataset("DISCGRP", "AWS.M2.CARDDEMO.DISCGRP.PS", "CVTRA02Y", "discgrp.txt", "DISCGRP"),
            new Dataset("EXPORT", "AWS.M2.CARDDEMO.EXPORT.DATA.PS", "CVEXPORT", null, null,
                    SampleDataRoundTripTest::exportVariant),
            new Dataset("TCATBALF", "AWS.M2.CARDDEMO.TCATBALF.PS", "CVTRA01Y", "tcatbal.txt", "TCATBALF"),
            new Dataset("TRANCATG", "AWS.M2.CARDDEMO.TRANCATG.PS", "CVTRA04Y", "trancatg.txt", "TRANCATG"),
            new Dataset("TRANTYPE", "AWS.M2.CARDDEMO.TRANTYPE.PS", "CVTRA03Y", "trantype.txt", "TRANTYPE"),
            new Dataset("USRSEC", "AWS.M2.CARDDEMO.USRSEC.PS", "CSUSR01Y", null, "USRSEC"));

    static Stream<Dataset> datasets() {
        return DATASETS.stream();
    }

    static Stream<Dataset> twinned() {
        return DATASETS.stream().filter(d -> d.ascii() != null);
    }

    @Test
    void everyEbcdicSampleFileIsCovered() throws IOException {
        try (Stream<Path> files = Files.list(TestData.resolve("app/data/EBCDIC"))) {
            Set<String> names = files.map(p -> p.getFileName().toString()).filter(n -> !n.startsWith("."))
                    .collect(Collectors.toCollection(TreeSet::new));
            assertThat(names).containsExactlyInAnyOrderElementsOf(DATASETS.stream().map(Dataset::ebcdic).toList());
        }
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("datasets")
    void decodesEveryFieldAndReencodesByteForByte(Dataset ds) {
        RecordLayout layout = ds.layout();
        List<FixedWidthRecord> records = ds.ebcdicRecords();
        assertThat(records).isNotEmpty();
        for (FixedWidthRecord rec : records) {
            String[] variant = ds.variant().apply(rec);
            Map<String, Object> values = layout.decode(rec, variant);
            FixedWidthRecord rebuilt = rec.copy();
            for (RecordLayout.Leaf leaf : layout.leaves(variant)) {
                if (!leaf.field().isFiller()) {
                    rebuilt.fill(leaf.field(), (byte) 0xFF);
                }
            }
            layout.encode(values, rebuilt, variant);
            assertThat(rebuilt.bytes()).as("%s %s", ds, rec).isEqualTo(rec.bytes());
        }
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("twinned")
    void matchesAsciiTwinFieldByField(Dataset ds) throws IOException {
        RecordLayout layout = ds.layout();
        List<FixedWidthRecord> ebcdic = ds.ebcdicRecords();
        List<FixedWidthRecord> ascii = ds.asciiRecords();
        assertThat(ebcdic).hasSameSizeAs(ascii);
        List<String> diffs = new ArrayList<>();
        for (int i = 0; i < ebcdic.size(); i++) {
            Map<String, Object> e = layout.decode(ebcdic.get(i));
            Map<String, Object> a = layout.decode(ascii.get(i));
            assertThat(e.keySet()).containsExactlyElementsOf(a.keySet());
            for (String key : e.keySet()) {
                if (!Objects.equals(e.get(key), a.get(key))) {
                    diffs.add(String.join("|", ds.id(), Integer.toString(i + 1), key, show(e.get(key)),
                            show(a.get(key))));
                }
            }
        }
        assertThat(diffs).containsExactlyElementsOf(expectedDiffs(ds.id()));
    }

    @Test
    void datasetsWithDiffsAreExactlyThoseTheBaselineFlagged() throws IOException {
        String readme = Files.readString(TestData.resolve("docs/validation/baseline/README.md"));
        Set<String> flagged = new TreeSet<>();
        Matcher m = Pattern.compile("(?m)^\\| ([A-Z.]+) \\|.*DIFFERS from ASCII sample").matcher(readme);
        while (m.find()) {
            flagged.add(m.group(1));
        }
        Set<String> withDiffs = new TreeSet<>();
        for (String line : expectedDiffLines()) {
            String id = line.substring(0, line.indexOf('|'));
            DATASETS.stream().filter(d -> d.id().equals(id)).forEach(d -> withDiffs.add(d.baseline()));
        }
        assertThat(flagged).isNotEmpty().isEqualTo(withDiffs);
    }

    /** The baseline's 00-DATA inputs equal our EBCDIC decode, except where it ran on the differing ASCII sample. */
    @ParameterizedTest(name = "{0}")
    @MethodSource("datasets")
    void decodedTextMatchesBaselineInputs(Dataset ds) throws IOException {
        if (ds.baseline() == null) {
            return;
        }
        Path file = TestData.resolve("docs/validation/baseline/00-DATA/" + ds.baseline() + ".txt");
        List<String> baseline = new ArrayList<>(List.of(unescape(Files.readString(file, StandardCharsets.ISO_8859_1))
                .split("\n", -1)));
        if (baseline.get(baseline.size() - 1).isEmpty()) {
            baseline.remove(baseline.size() - 1);
        }
        boolean differs = expectedDiffLines().stream().anyMatch(l -> l.startsWith(ds.id() + "|"));
        List<String> ours = (differs ? ds.asciiRecords() : ds.ebcdicRecords()).stream()
                .map(FixedWidthRecord::text).toList();
        assertThat(ours).containsExactlyElementsOf(baseline);
    }

    private static String unescape(String text) {
        Matcher m = Pattern.compile("\\\\x([0-9a-fA-F]{2})").matcher(text);
        StringBuilder out = new StringBuilder();
        while (m.find()) {
            m.appendReplacement(out, Matcher.quoteReplacement(
                    String.valueOf((char) Integer.parseInt(m.group(1), 16))));
        }
        return m.appendTail(out).toString();
    }

    private static String show(Object value) {
        return value instanceof BigDecimal d ? d.toPlainString() : String.valueOf(value).stripTrailing();
    }

    private static List<String> expectedDiffs(String id) throws IOException {
        return expectedDiffLines().stream().filter(l -> l.startsWith(id + "|")).toList();
    }

    private static List<String> expectedDiffLines() throws IOException {
        try (InputStream in = SampleDataRoundTripTest.class.getResourceAsStream(
                "/codec/ebcdic-vs-ascii-expected-diffs.txt")) {
            return new String(Objects.requireNonNull(in).readAllBytes(), StandardCharsets.UTF_8).lines()
                    .filter(l -> !l.isBlank() && !l.startsWith("#")).toList();
        }
    }
}
