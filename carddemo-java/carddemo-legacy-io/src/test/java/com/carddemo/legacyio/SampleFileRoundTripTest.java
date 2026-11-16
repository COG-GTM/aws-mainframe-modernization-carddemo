package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/** Every sample file in {@code app/data} decodes and re-encodes to the same bytes. */
class SampleFileRoundTripTest {

    private static final Map<String, CardDemoFile> ASCII_FILES = Map.of(
            "acctdata.txt", CardDemoFile.ACCOUNT,
            "carddata.txt", CardDemoFile.CARD,
            "cardxref.txt", CardDemoFile.CARD_XREF,
            "custdata.txt", CardDemoFile.CUSTOMER,
            "dailytran.txt", CardDemoFile.DAILY_TRANSACTION,
            "discgrp.txt", CardDemoFile.DISCLOSURE_GROUP,
            "tcatbal.txt", CardDemoFile.TRAN_CAT_BALANCE,
            "trancatg.txt", CardDemoFile.TRAN_CATEGORY,
            "trantype.txt", CardDemoFile.TRAN_TYPE);

    private static final Map<String, CardDemoFile> EBCDIC_FILES = Map.ofEntries(
            Map.entry("AWS.M2.CARDDEMO.ACCDATA.PS", CardDemoFile.ACCOUNT),
            Map.entry("AWS.M2.CARDDEMO.ACCTDATA.PS", CardDemoFile.ACCOUNT),
            Map.entry("AWS.M2.CARDDEMO.CARDDATA.PS", CardDemoFile.CARD),
            Map.entry("AWS.M2.CARDDEMO.CARDXREF.PS", CardDemoFile.CARD_XREF),
            Map.entry("AWS.M2.CARDDEMO.CUSTDATA.PS", CardDemoFile.CUSTOMER),
            Map.entry("AWS.M2.CARDDEMO.DALYTRAN.PS", CardDemoFile.DAILY_TRANSACTION),
            Map.entry("AWS.M2.CARDDEMO.DALYTRAN.PS.INIT", CardDemoFile.DAILY_TRANSACTION),
            Map.entry("AWS.M2.CARDDEMO.DISCGRP.PS", CardDemoFile.DISCLOSURE_GROUP),
            Map.entry("AWS.M2.CARDDEMO.EXPORT.DATA.PS", CardDemoFile.EXPORT),
            Map.entry("AWS.M2.CARDDEMO.TCATBALF.PS", CardDemoFile.TRAN_CAT_BALANCE),
            Map.entry("AWS.M2.CARDDEMO.TRANCATG.PS", CardDemoFile.TRAN_CATEGORY),
            Map.entry("AWS.M2.CARDDEMO.TRANTYPE.PS", CardDemoFile.TRAN_TYPE),
            Map.entry("AWS.M2.CARDDEMO.USRSEC.PS", CardDemoFile.USER_SECURITY));

    static Stream<Arguments> sampleFiles() {
        return Stream.concat(
                ASCII_FILES.entrySet().stream()
                        .map(e -> Arguments.of(Encoding.ASCII, e.getKey(), e.getValue())),
                EBCDIC_FILES.entrySet().stream()
                        .map(e -> Arguments.of(Encoding.EBCDIC, e.getKey(), e.getValue())));
    }

    @ParameterizedTest(name = "{0} {1}")
    @MethodSource("sampleFiles")
    void roundTripsByteForByte(Encoding encoding, String fileName, CardDemoFile file) throws IOException {
        Path path = encoding == Encoding.ASCII ? SampleData.ascii(fileName) : SampleData.ebcdic(fileName);
        byte[] original = Files.readAllBytes(path);

        List<Record> records = LegacyFile.read(path, file, encoding);
        byte[] written = LegacyFile.toBytes(records, encoding, RecordFormat.defaultFor(encoding));

        assertThat(records).isNotEmpty();
        assertThat(written).isEqualTo(original);
    }

    @ParameterizedTest(name = "{0} {1}")
    @MethodSource("sampleFiles")
    void numericFieldsHoldValidData(Encoding encoding, String fileName, CardDemoFile file) throws IOException {
        Path path = encoding == Encoding.ASCII ? SampleData.ascii(fileName) : SampleData.ebcdic(fileName);

        List<Record> records = LegacyFile.read(path, file, encoding);

        if (fileName.endsWith(".INIT")) {
            // DALYTRAN.PS.INIT is a single LOW-VALUES record used to initialise the dataset.
            assertThat(records).singleElement().satisfies(r -> assertThat(r.invalidFields()).isNotEmpty());
            return;
        }
        assertThat(records).allSatisfy(r -> assertThat(r.invalidFields()).as(r.toString()).isEmpty());
    }

    /** The two sample sets hold the same records (a few non-key values differ between them, e.g. ACCT-ADDR-ZIP). */
    @ParameterizedTest(name = "{1}")
    @MethodSource("sampleFiles")
    void asciiAndEbcdicSamplesHoldTheSameKeys(Encoding encoding, String fileName, CardDemoFile file)
            throws IOException {
        String asciiName = ASCII_FILES.entrySet().stream()
                .filter(e -> e.getValue() == file).map(Map.Entry::getKey).findFirst().orElse(null);
        if (encoding != Encoding.EBCDIC || asciiName == null || fileName.endsWith(".INIT")
                || fileName.contains("ACCDATA")) {
            return;
        }
        List<Record> ebcdic = LegacyFile.read(SampleData.ebcdic(fileName), file, Encoding.EBCDIC);
        List<Record> ascii = LegacyFile.read(SampleData.ascii(asciiName), file, Encoding.ASCII);

        assertThat(ebcdic).hasSameSizeAs(ascii);
        String key = file.layout().fields().get(0).name();
        for (int i = 0; i < ascii.size(); i++) {
            assertThat(ebcdic.get(i).get(key)).as("%s record %d %s", fileName, i + 1, key)
                    .isEqualTo(ascii.get(i).get(key));
        }
    }

    @Test
    void everySampleFileIsCovered() throws IOException {
        assertThat(listFiles(SampleData.appDir().resolve("data/ASCII"))).isEqualTo(new TreeSet<>(ASCII_FILES.keySet()));
        assertThat(listFiles(SampleData.appDir().resolve("data/EBCDIC")))
                .isEqualTo(new TreeSet<>(EBCDIC_FILES.keySet()));
    }

    @Test
    void decodesAccountValues() throws IOException {
        Record first = LegacyFile.read(SampleData.ebcdic("AWS.M2.CARDDEMO.ACCTDATA.PS"), CardDemoFile.ACCOUNT,
                Encoding.EBCDIC).get(0);

        assertThat(first.getLong("ACCT-ID")).isEqualTo(1);
        assertThat(first.getString("ACCT-ACTIVE-STATUS")).isEqualTo("Y");
        assertThat(first.getDecimal("ACCT-CURR-BAL").scale()).isEqualTo(2);
        assertThat(first.getString("ACCT-OPEN-DATE")).matches("\\d{4}-\\d{2}-\\d{2}");
    }

    @Test
    void decodesNegativeDailyTransactionAmounts() throws IOException {
        List<Record> records = LegacyFile.read(SampleData.ascii("dailytran.txt"), CardDemoFile.DAILY_TRANSACTION,
                Encoding.ASCII);

        assertThat(records).hasSize(300);
        assertThat(records).anySatisfy(r -> assertThat(r.getDecimal("DALYTRAN-AMT")).isNegative());
    }

    @Test
    void decodesEveryExportRecordType() throws IOException {
        List<Record> records = LegacyFile.read(SampleData.ebcdic("AWS.M2.CARDDEMO.EXPORT.DATA.PS"),
                CardDemoFile.EXPORT, Encoding.EBCDIC);

        Set<String> types = records.stream().map(r -> r.getString("EXPORT-REC-TYPE")).collect(Collectors.toSet());
        assertThat(types).containsExactlyInAnyOrder("C", "A", "X", "T", "D");
        Record account = records.stream().filter(r -> r.getString("EXPORT-REC-TYPE").equals("A")).findFirst()
                .orElseThrow();
        assertThat(account.getDecimal("EXP-ACCT-CURR-BAL")).isInstanceOf(BigDecimal.class);
        assertThat(account.getLong("EXP-ACCT-ID")).isPositive();
    }

    @Test
    void editedRecordsAreWrittenInFull() throws IOException {
        List<Record> records = LegacyFile.read(SampleData.ascii("cardxref.txt"), CardDemoFile.CARD_XREF,
                Encoding.ASCII);
        records.get(0).set("FILLER@36", "X".repeat(14));

        byte[] written = LegacyFile.toBytes(records, Encoding.ASCII, RecordFormat.LINE_SEQUENTIAL);
        List<Record> reread = LegacyFile.read(new ByteArrayInputStream(written), CardDemoFile.CARD_XREF,
                Encoding.ASCII, RecordFormat.LINE_SEQUENTIAL);

        assertThat(reread.get(0).framing().length()).isEqualTo(50);
        assertThat(reread.get(0).getString("FILLER@36")).isEqualTo("X".repeat(14));
        assertThat(reread.get(1).values()).isEqualTo(records.get(1).values());
    }

    private static Set<String> listFiles(Path dir) throws IOException {
        try (Stream<Path> files = Files.list(dir)) {
            return files.filter(Files::isRegularFile).map(p -> p.getFileName().toString())
                    .filter(name -> !name.startsWith("."))
                    .collect(Collectors.toCollection(TreeSet::new));
        } catch (UncheckedIOException e) {
            throw e.getCause();
        }
    }
}
