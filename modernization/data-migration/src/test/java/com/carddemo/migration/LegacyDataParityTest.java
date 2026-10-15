package com.carddemo.migration;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Decodes every shipped VSAM unload twice - from the EBCDIC REPRO output and from the ASCII copy - and asserts both
 * conversions agree field by field. This proves the EBCDIC (cp037) and zoned/overpunch rules on real CardDemo data.
 */
class LegacyDataParityTest {

    static final Path DATA = Path.of(System.getProperty("carddemo.legacy.data", "../../app/data"));

    /**
     * The two sample copies shipped in app/data are not byte-identical in content. EBCDIC (the REPRO unload) is the
     * system of record; these divergences are documented in docs/modernization/03-data-migration-runbook.md.
     */
    static final Map<String, List<String>> KNOWN_SAMPLE_DIVERGENCES = Map.of(
            "CVACT01Y", List.of("CVACT01Y#48.addr_zip ascii=A000000000 ebcdic=ZEROAPR"),
            "CVTRA02Y", List.of("CVTRA02Y#33.int_rate ascii=0 ebcdic=15"));

    static List<VsamToRdsLoader.Source> sources() {
        return VsamToRdsLoader.PLAN.stream().filter(s -> s.asciiFile() != null && !s.optional()).toList();
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("sources")
    void ebcdicAndAsciiUnloadsDecodeIdentically(VsamToRdsLoader.Source source) throws Exception {
        List<Map<String, Object>> ebcdic = decode(source, SourceEncoding.EBCDIC);
        List<Map<String, Object>> ascii = decode(source, SourceEncoding.ASCII);
        assertThat(ebcdic).isNotEmpty();
        assertThat(ascii).hasSameSizeAs(ebcdic);
        List<String> differences = new java.util.ArrayList<>();
        for (int i = 0; i < ebcdic.size(); i++) {
            Map<String, Object> a = normalise(ascii.get(i));
            Map<String, Object> e = normalise(ebcdic.get(i));
            for (String column : e.keySet()) {
                if (!java.util.Objects.equals(a.get(column), e.get(column))) {
                    differences.add(source.layout().copybook() + "#" + i + "." + column + " ascii=" + a.get(column)
                            + " ebcdic=" + e.get(column));
                }
            }
        }
        assertThat(differences).as("ASCII vs EBCDIC differences")
                .containsExactlyInAnyOrderElementsOf(KNOWN_SAMPLE_DIVERGENCES.getOrDefault(
                        source.layout().copybook(), List.of()));
    }

    @org.junit.jupiter.api.Test
    void decodesFirstAccountRecordExactly() throws Exception {
        Map<String, Object> first = decode(VsamToRdsLoader.PLAN.get(4), SourceEncoding.ASCII).get(0);
        assertThat(first.get("acct_id")).isEqualTo(1L);
        assertThat(first.get("active_status")).isEqualTo("Y");
        assertThat((BigDecimal) first.get("curr_bal")).isEqualByComparingTo("194.00");
        assertThat((BigDecimal) first.get("credit_limit")).isEqualByComparingTo("2020.00");
        assertThat((BigDecimal) first.get("cash_credit_limit")).isEqualByComparingTo("1020.00");
        assertThat(first.get("open_date")).isEqualTo(LocalDate.of(2014, 11, 20));
        assertThat(first.get("expiration_date")).isEqualTo(LocalDate.of(2025, 5, 20));
        // Sample data quirk: the disclosure-group code sits in ACCT-ADDR-ZIP and ACCT-GROUP-ID is blank.
        assertThat(first.get("addr_zip")).isEqualTo("A000000000");
        assertThat(first.get("group_id")).isNull();
    }

    @org.junit.jupiter.api.Test
    void usrsecEbcdicMatchesDusrsecjSeed() throws Exception {
        List<byte[]> ebcdic;
        try (InputStream in = Files.newInputStream(DATA.resolve("EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS"))) {
            ebcdic = RecordReader.read(in, 80, SourceEncoding.EBCDIC);
        }
        List<byte[]> seed;
        try (InputStream in = getClass().getResourceAsStream("/seed/usrsec.txt")) {
            seed = RecordReader.read(in, 80, SourceEncoding.ASCII);
        }
        assertThat(seed).hasSameSizeAs(ebcdic);
        for (int i = 0; i < seed.size(); i++) {
            assertThat(RecordDecoder.decode(seed.get(i), CopybookLayouts.USER_SECURITY, SourceEncoding.ASCII))
                    .isEqualTo(RecordDecoder.decode(ebcdic.get(i), CopybookLayouts.USER_SECURITY,
                            SourceEncoding.EBCDIC));
        }
    }

    private static List<Map<String, Object>> decode(VsamToRdsLoader.Source source, SourceEncoding encoding)
            throws Exception {
        Path file = encoding == SourceEncoding.EBCDIC
                ? DATA.resolve("EBCDIC").resolve(source.ebcdicFile())
                : DATA.resolve("ASCII").resolve(source.asciiFile());
        try (InputStream in = Files.newInputStream(file)) {
            return RecordReader.read(in, source.layout().recordLength(), encoding).stream()
                    .map(r -> RecordDecoder.decode(r, source.layout(), encoding))
                    .toList();
        }
    }

    private static Map<String, Object> normalise(Map<String, Object> row) {
        Map<String, Object> copy = new java.util.LinkedHashMap<>();
        row.forEach((k, v) -> copy.put(k, v instanceof BigDecimal b ? b.stripTrailingZeros().toPlainString() : v));
        return copy;
    }
}
