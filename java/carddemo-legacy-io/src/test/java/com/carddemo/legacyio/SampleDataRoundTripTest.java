package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/** Every file in {@code app/data/ASCII} and {@code app/data/EBCDIC} must decode and re-encode byte for byte. */
class SampleDataRoundTripTest {

    private static final Path DATA = Path.of("..", "..", "app", "data");

    private record Sample(String directory, String file, int recordLength, LayoutSelector selector) {
        Path path() {
            return DATA.resolve(directory).resolve(file);
        }

        LegacyFileCodec codec() {
            return directory.equals("ASCII") ? LegacyFileCodec.ascii() : LegacyFileCodec.ebcdic();
        }

        @Override
        public String toString() {
            return directory + "/" + file;
        }
    }

    private static Sample sample(String directory, String file, RecordLayout layout) {
        return new Sample(directory, file, layout.length(), LayoutSelector.of(layout));
    }

    private static final List<Sample> SAMPLES = List.of(
            sample("ASCII", "acctdata.txt", CardDemoLayouts.ACCOUNT),
            sample("ASCII", "carddata.txt", CardDemoLayouts.CARD),
            sample("ASCII", "cardxref.txt", CardDemoLayouts.CARD_XREF),
            sample("ASCII", "custdata.txt", CardDemoLayouts.CUSTOMER),
            sample("ASCII", "dailytran.txt", CardDemoLayouts.DAILY_TRANSACTION),
            sample("ASCII", "discgrp.txt", CardDemoLayouts.DISCLOSURE_GROUP),
            sample("ASCII", "tcatbal.txt", CardDemoLayouts.TRAN_CAT_BALANCE),
            sample("ASCII", "trancatg.txt", CardDemoLayouts.TRAN_CATEGORY),
            sample("ASCII", "trantype.txt", CardDemoLayouts.TRAN_TYPE),
            sample("EBCDIC", "AWS.M2.CARDDEMO.ACCDATA.PS", CardDemoLayouts.ACCOUNT),
            sample("EBCDIC", "AWS.M2.CARDDEMO.ACCTDATA.PS", CardDemoLayouts.ACCOUNT),
            sample("EBCDIC", "AWS.M2.CARDDEMO.CARDDATA.PS", CardDemoLayouts.CARD),
            sample("EBCDIC", "AWS.M2.CARDDEMO.CARDXREF.PS", CardDemoLayouts.CARD_XREF),
            sample("EBCDIC", "AWS.M2.CARDDEMO.CUSTDATA.PS", CardDemoLayouts.CUSTOMER),
            sample("EBCDIC", "AWS.M2.CARDDEMO.DALYTRAN.PS", CardDemoLayouts.DAILY_TRANSACTION),
            sample("EBCDIC", "AWS.M2.CARDDEMO.DALYTRAN.PS.INIT", CardDemoLayouts.DAILY_TRANSACTION),
            sample("EBCDIC", "AWS.M2.CARDDEMO.DISCGRP.PS", CardDemoLayouts.DISCLOSURE_GROUP),
            new Sample("EBCDIC", "AWS.M2.CARDDEMO.EXPORT.DATA.PS", CardDemoLayouts.EXPORT.length(),
                    CardDemoLayouts.EXPORT_BY_TYPE),
            sample("EBCDIC", "AWS.M2.CARDDEMO.TCATBALF.PS", CardDemoLayouts.TRAN_CAT_BALANCE),
            sample("EBCDIC", "AWS.M2.CARDDEMO.TRANCATG.PS", CardDemoLayouts.TRAN_CATEGORY),
            sample("EBCDIC", "AWS.M2.CARDDEMO.TRANTYPE.PS", CardDemoLayouts.TRAN_TYPE),
            sample("EBCDIC", "AWS.M2.CARDDEMO.USRSEC.PS", CardDemoLayouts.USER_SECURITY));

    static Stream<Arguments> samples() {
        return SAMPLES.stream().map(Arguments::of);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("samples")
    void roundTripsByteForByte(Sample sample) throws IOException {
        byte[] original = Files.readAllBytes(sample.path());
        LegacyFileCodec codec = sample.codec();

        List<FileRecord> records = codec.read(original, sample.recordLength(), sample.selector());

        assertThat(records).isNotEmpty();
        assertThat(codec.write(records)).isEqualTo(original);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("samples")
    void reEncodingDecodedValuesReproducesTheFile(Sample sample) throws IOException {
        byte[] original = Files.readAllBytes(sample.path());
        LegacyFileCodec codec = sample.codec();
        List<FileRecord> rebuilt = codec.read(original, sample.recordLength(), sample.selector()).stream()
                .map(r -> new FileRecord(copy(r.record()), r.storedLength(), r.ending()))
                .toList();

        assertThat(codec.write(rebuilt)).isEqualTo(original);
    }

    @Test
    void everySampleFileIsCovered() throws IOException {
        Set<String> onDisk = new HashSet<>();
        for (String directory : List.of("ASCII", "EBCDIC")) {
            try (Stream<Path> files = Files.list(DATA.resolve(directory))) {
                files.map(Path::getFileName).map(Path::toString).filter(f -> !f.startsWith("."))
                        .forEach(f -> onDisk.add(directory + "/" + f));
            }
        }
        Set<String> covered = new HashSet<>();
        SAMPLES.forEach(s -> covered.add(s.toString()));
        assertThat(covered).isEqualTo(onDisk);
    }

    @Test
    void asciiAndEbcdicAccountsDecodeToTheSameValues() throws IOException {
        List<FileRecord> ascii = LegacyFileCodec.ascii().read(DATA.resolve("ASCII/acctdata.txt"), CardDemoLayouts.ACCOUNT);
        List<FileRecord> ebcdic = LegacyFileCodec.ebcdic()
                .read(DATA.resolve("EBCDIC/AWS.M2.CARDDEMO.ACCTDATA.PS"), CardDemoLayouts.ACCOUNT);

        assertThat(ascii).hasSize(50);
        assertThat(ebcdic).hasSize(50);
        for (int i = 0; i < 50; i++) {
            Map<String, Object> asciiValues = ascii.get(i).record().toMap();
            Map<String, Object> ebcdicValues = ebcdic.get(i).record().toMap();
            if (i == 48) {
                // the two extracts disagree only on account 49's ACCT-ADDR-ZIP: A000000000 vs ZEROAPR
                asciiValues.remove("ACCT-ADDR-ZIP");
                assertThat(ebcdicValues.remove("ACCT-ADDR-ZIP")).isEqualTo("ZEROAPR   ");
            }
            assertThat(ebcdicValues).as("account %d", i + 1).isEqualTo(asciiValues);
        }
        Record first = ascii.getFirst().record();
        assertThat(first.getLong("ACCT-ID")).isEqualTo(1);
        assertThat(first.getString("ACCT-ACTIVE-STATUS")).isEqualTo("Y");
        assertThat(first.getDecimal("ACCT-CURR-BAL")).isEqualByComparingTo("194.00");
        assertThat(first.getDecimal("ACCT-CREDIT-LIMIT")).isEqualByComparingTo("2020.00");
        assertThat(first.getString("ACCT-OPEN-DATE")).isEqualTo("2014-11-20");
        assertThat(first.invalidFields()).isEmpty();
    }

    @Test
    void decodesNegativeOverpunchedAmounts() throws IOException {
        List<Record> daily = LegacyFileCodec.ascii()
                .read(DATA.resolve("ASCII/dailytran.txt"), CardDemoLayouts.DAILY_TRANSACTION)
                .stream().map(FileRecord::record).toList();

        assertThat(daily.getFirst().getDecimal("DALYTRAN-AMT")).isEqualByComparingTo("504.77");
        assertThat(daily).anySatisfy(r -> assertThat(r.getDecimal("DALYTRAN-AMT").signum()).isNegative());
        assertThat(daily).allSatisfy(r -> assertThat(r.invalidFields()).isEmpty());
    }

    @Test
    void keepsShortAsciiLinesAndCrlfEndings() throws IOException {
        List<FileRecord> xref = LegacyFileCodec.ascii().read(DATA.resolve("ASCII/cardxref.txt"), CardDemoLayouts.CARD_XREF);
        assertThat(xref).allSatisfy(r -> {
            assertThat(r.storedLength()).isEqualTo(36);
            assertThat(r.ending()).isEqualTo(LineEnding.LF);
        });
        assertThat(xref.getFirst().record().getString("XREF-CARD-NUM")).isEqualTo("0500024453765740");
        assertThat(xref.getFirst().record().getLong("XREF-ACCT-ID")).isEqualTo(50);

        List<FileRecord> categories = LegacyFileCodec.ascii()
                .read(DATA.resolve("ASCII/trancatg.txt"), CardDemoLayouts.TRAN_CATEGORY);
        assertThat(categories).allSatisfy(r -> assertThat(r.ending()).isEqualTo(LineEnding.CRLF));
    }

    @Test
    void decodesEveryExportRecordType() throws IOException {
        List<Record> export = LegacyFileCodec.ebcdic()
                .read(DATA.resolve("EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS"), 500, CardDemoLayouts.EXPORT_BY_TYPE)
                .stream().map(FileRecord::record).toList();

        assertThat(export).hasSize(500);
        Map<String, Long> byType = new java.util.TreeMap<>();
        export.forEach(r -> byType.merge(r.getString("EXPORT-REC-TYPE"), 1L, Long::sum));
        assertThat(byType.keySet()).containsExactly("A", "C", "D", "T", "X");
        assertThat(export).allSatisfy(r -> {
            assertThat(r.layout()).isNotSameAs(CardDemoLayouts.EXPORT);
            assertThat(r.invalidFields()).isEmpty();
        });

        Record account = export.stream().filter(r -> r.getString("EXPORT-REC-TYPE").equals("A")).findFirst().orElseThrow();
        assertThat(account.getLong("EXP-ACCT-ID")).isPositive();
        assertThat(account.getDecimal("EXP-ACCT-CURR-BAL").scale()).isEqualTo(2);
        assertThat(account.getString("EXP-ACCT-OPEN-DATE")).matches("\\d{4}-\\d{2}-\\d{2}");
    }

    @Test
    void writesNewRecordsInBothFramings() {
        Record user = Record.initialized(CardDemoLayouts.USER_SECURITY)
                .set("SEC-USR-ID", "ADMIN001")
                .set("SEC-USR-TYPE", "A");
        byte[] ascii = LegacyFileCodec.ascii().writeRecords(List.of(user));
        byte[] ebcdic = LegacyFileCodec.ebcdic().writeRecords(List.of(user));
        assertThat(ascii).hasSize(81);
        assertThat(ascii[80]).isEqualTo((byte) '\n');
        assertThat(ebcdic).hasSize(80);
        assertThat(LegacyFileCodec.ebcdic().read(ebcdic, CardDemoLayouts.USER_SECURITY).getFirst().record())
                .isEqualTo(user);
    }

    private static Record copy(Record source) {
        Record target = Record.initialized(source.layout());
        for (int i = 0; i < source.layout().fields().size(); i++) {
            Object value = source.get(i);
            target.set(i, value instanceof BigDecimal d ? new BigDecimal(d.unscaledValue(), d.scale()) : value);
        }
        return target;
    }
}
