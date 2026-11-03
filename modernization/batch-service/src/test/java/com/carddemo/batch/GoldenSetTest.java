package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

/**
 * Runs each job on the sample files the COBOL reference ran on (scripts/migration/golden/batch-service-reference.sh,
 * GnuCOBOL, clock fixed at 2022-07-18 10:30:45 UTC) and compares every output DD, SYSOUT and the return code
 * byte for byte with {@code src/test/resources/golden/<SET>/expected}.
 */
@SpringBootTest(properties = "spring.main.allow-bean-definition-overriding=true")
class GoldenSetTest {

    static final Path DATA = Path.of("../../app/data/ASCII");
    static final Path GOLDEN = Path.of("src/test/resources/golden");

    @TestConfiguration
    static class FixedClock {
        @Bean
        @Primary
        Clock goldenClock() {
            return Clock.fixed(Instant.parse("2022-07-18T10:30:45Z"), ZoneOffset.UTC);
        }
    }

    @Autowired
    private BatchJobLauncher launcher;

    @TempDir
    Path out;

    @ParameterizedTest(name = "{0}")
    @CsvSource(delimiter = '|', value = {
            "READACCT      | READACCT | ACCTFILE=DATA/acctdata.txt",
            "READACCT-EDGE | READACCT | ACCTFILE=SET/input/acctdata.txt",
            "READCARD      | READCARD | CARDFILE=DATA/carddata.txt",
            "READXREF      | READXREF | XREFFILE=DATA/cardxref.txt",
            "READCUST      | READCUST | CUSTFILE=DATA/custdata.txt",
            "CBEXPORT      | CBEXPORT | CUSTFILE=DATA/custdata.txt ACCTFILE=DATA/acctdata.txt XREFFILE=DATA/cardxref.txt TRANSACT=DATA/dailytran.txt CARDFILE=DATA/carddata.txt",
            "CBIMPORT      | CBIMPORT | EXPFILE=GOLDEN/CBEXPORT/expected/EXPFILE",
            "CBIMPORT-EDGE | CBIMPORT | EXPFILE=SET/input/EXPFILE",
            "CREASTMT      | CREASTMT | TRANSACT=DATA/dailytran.txt XREFFILE=DATA/cardxref.txt CUSTFILE=DATA/custdata.txt ACCTFILE=DATA/acctdata.txt",
    })
    void matchesTheCobolReference(String set, String job, String dds) throws Exception {
        Path expected = GOLDEN.resolve(set).resolve("expected");
        List<String> args = new ArrayList<>(List.of("--job=" + job, "--output-dir=" + out));
        for (String dd : dds.trim().split(" ")) {
            args.add("--dd=" + dd.replace("DATA/", DATA + "/").replace("SET/", GOLDEN.resolve(set) + "/")
                    .replace("GOLDEN/", GOLDEN + "/"));
        }
        BatchRun run = launcher.launch(BatchArguments.parse(args.toArray(String[]::new)).orElseThrow());

        assertThat(run.returnCode()).isZero();
        List<Path> files;
        try (Stream<Path> listing = Files.list(expected)) {
            files = listing.sorted().toList();
        }
        for (Path golden : files) {
            String dd = golden.getFileName().toString();
            Path actual = run.outputDir().resolve(dd);
            byte[] want = Files.readAllBytes(golden);
            byte[] got = Files.exists(actual) ? Files.readAllBytes(actual) : new byte[0];
            if (dd.equals("VBRCFILE")) {
                got = gnuCobolVariable(got);
            }
            assertThat(new String(got, java.nio.charset.StandardCharsets.ISO_8859_1))
                    .as(set + " " + dd)
                    .isEqualTo(new String(want, java.nio.charset.StandardCharsets.ISO_8859_1));
        }
    }

    /**
     * GnuCOBOL writes a RECFM=V record with a 4-byte header holding the length without the header; the job
     * writes the z/OS RDW (length including the RDW). Same records otherwise.
     */
    static byte[] gnuCobolVariable(byte[] rdwFile) throws IOException {
        ByteArrayOutputStream converted = new ByteArrayOutputStream();
        int at = 0;
        while (at < rdwFile.length) {
            int length = ((rdwFile[at] & 0xFF) << 8 | (rdwFile[at + 1] & 0xFF)) - 4;
            converted.write(length >> 8);
            converted.write(length & 0xFF);
            converted.write(0);
            converted.write(0);
            converted.write(rdwFile, at + 4, length);
            at += 4 + length;
        }
        return converted.toByteArray();
    }
}
