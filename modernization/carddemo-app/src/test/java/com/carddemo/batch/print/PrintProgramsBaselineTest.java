package com.carddemo.batch.print;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.FixedFileSink;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.harness.VariableFileSink;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.AbendException;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.codec.TestData;
import com.carddemo.common.file.RecordPrefix;
import com.carddemo.customer.CustomerRecord;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * The four print programs on the inputs the GnuCOBOL baseline used ({@code app/data/ASCII}), compared with
 * {@code docs/validation/baseline/<JOB>/} by the rules of {@code scripts/batch/compare_print_jobs.py}: SYSOUT after
 * trailing-space normalisation, datasets byte for byte (rendered as the baseline renders them).
 */
public class PrintProgramsBaselineTest {

    @TempDir
    Path dir;

    static Stream<Arguments> recordPrints() {
        return Stream.of(
                Arguments.of("READCARD", RecordPrintProgram.Program.CBACT02C, "carddata.txt", CardRecord.MAPPER.layout()),
                Arguments.of("READXREF", RecordPrintProgram.Program.CBACT03C, "cardxref.txt",
                        CardXrefRecord.MAPPER.layout()),
                Arguments.of("READCUST", RecordPrintProgram.Program.CBCUS01C, "custdata.txt",
                        CustomerRecord.MAPPER.layout()));
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("recordPrints")
    void recordPrintMatchesTheBaseline(String job, RecordPrintProgram.Program program, String sample,
                                       RecordLayout layout) throws IOException {
        Path sysoutPath = dir.resolve("sysout.txt");
        ProgramCounts counts;
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            counts = new RecordPrintProgram(program, KsdsInput.file(program.ddname(),
                    TestData.resolve("app/data/ASCII/" + sample), layout, RecordEncoding.ASCII), sysout).run();
        }
        assertThat(counts.read()).isEqualTo(50);
        assertThat(sysoutLines(sysoutPath)).containsExactlyElementsOf(baselineSysout(job));
    }

    @Test
    void cbact01cMatchesTheBaselineSysoutAndDatasets() throws IOException {
        Path sysoutPath = dir.resolve("sysout.txt");
        ProgramCounts counts;
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            counts = new Cbact01c(
                    KsdsInput.file(Cbact01c.ACCTFILE, TestData.resolve("app/data/ASCII/acctdata.txt"),
                            AccountRecord.MAPPER.layout(), RecordEncoding.ASCII),
                    new FixedFileSink(Cbact01c.OUTFILE, dir.resolve("OUTFILE")),
                    new FixedFileSink(Cbact01c.ARRYFILE, dir.resolve("ARRYFILE")),
                    new VariableFileSink(Cbact01c.VBRCFILE, dir.resolve("VBRCFILE"), RecordPrefix.GNUCOBOL_VARSEQ_0,
                            Cbact01c.VBRC_MIN, Cbact01c.VBRC_MAX),
                    sysout, RecordEncoding.ASCII).run();
        }
        assertThat(counts).isEqualTo(new ProgramCounts(50, 200));
        assertThat(sysoutLines(sysoutPath)).containsExactlyElementsOf(baselineSysout("READACCT"));
        assertThat(fold(dir.resolve("OUTFILE"), 107)).containsExactlyElementsOf(baselineFile("READACCT", "OUTFILE"));
        assertThat(fold(dir.resolve("ARRYFILE"), 110)).containsExactlyElementsOf(baselineFile("READACCT", "ARRYFILE"));
        assertThat(foldVarseq0(dir.resolve("VBRCFILE"))).containsExactlyElementsOf(baselineFile("READACCT", "VBRCFILE"));
    }

    @Test
    void cbact01cWritesEbcdicWithRdwsWhenAskedTo() throws IOException {
        try (Sysout sysout = Sysout.open(dir.resolve("sysout.txt"))) {
            new Cbact01c(
                    KsdsInput.file(Cbact01c.ACCTFILE, TestData.resolve("app/data/ASCII/acctdata.txt"),
                            AccountRecord.MAPPER.layout(), RecordEncoding.ASCII),
                    new FixedFileSink(Cbact01c.OUTFILE, dir.resolve("OUTFILE")),
                    new FixedFileSink(Cbact01c.ARRYFILE, dir.resolve("ARRYFILE")),
                    new VariableFileSink(Cbact01c.VBRCFILE, dir.resolve("VBRCFILE"), RecordPrefix.ZOS_RDW,
                            Cbact01c.VBRC_MIN, Cbact01c.VBRC_MAX),
                    sysout, RecordEncoding.EBCDIC).run();
        }
        byte[] vb = Files.readAllBytes(dir.resolve("VBRCFILE"));
        assertThat(Arrays.copyOf(vb, 4)).containsExactly(0, 16, 0, 0);
        assertThat(new String(vb, 4, 12, RecordEncoding.EBCDIC.charset())).isEqualTo("00000000001Y");
        assertThat(Files.size(dir.resolve("OUTFILE"))).isEqualTo(50L * 107);
    }

    @Test
    void aMissingInputTakesTheAbendPath() throws IOException {
        Path sysoutPath = dir.resolve("sysout.txt");
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            RecordPrintProgram program = new RecordPrintProgram(RecordPrintProgram.Program.CBACT02C,
                    KsdsInput.file("CARDFILE", dir.resolve("missing"), CardRecord.MAPPER.layout(), RecordEncoding.ASCII),
                    sysout);
            assertThatThrownBy(program::run).isInstanceOf(AbendException.class)
                    .hasMessageContaining("ERROR OPENING CARDFILE");
        }
        assertThat(sysoutLines(sysoutPath)).containsExactly("START OF EXECUTION OF PROGRAM CBACT02C",
                "ERROR OPENING CARDFILE", "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM");
    }

    // --- rendering and normalisation of scripts/baseline/baseline.py and scripts/batch/compare_print_jobs.py ---

    public static String render(byte[] data, int from, int to) {
        StringBuilder sb = new StringBuilder();
        for (int i = from; i < to; i++) {
            int b = data[i] & 0xFF;
            sb.append(b >= 0x20 && b <= 0x7E ? String.valueOf((char) b) : String.format("\\x%02x", b));
        }
        return sb.toString();
    }

    public static List<String> sysoutLines(Path path) throws IOException {
        byte[] data = Files.readAllBytes(path);
        List<String> lines = new ArrayList<>();
        int start = 0;
        for (int i = 0; i < data.length; i++) {
            if (data[i] == '\n') {
                lines.add(render(data, start, i).stripTrailing());
                start = i + 1;
            }
        }
        return lines;
    }

    public static List<String> baselineSysout(String job) throws IOException {
        return Files.readAllLines(TestData.resolve("docs/validation/baseline/" + job + "/sysout.txt"),
                        StandardCharsets.ISO_8859_1).stream()
                .filter(l -> !l.startsWith("--- EXEC ") && !l.startsWith("libcob: ") && !l.matches("rc=-?\\d+"))
                .map(l -> l.replaceAll(" +$", ""))
                .toList();
    }

    public static List<String> baselineFile(String job, String dd) throws IOException {
        return Files.readAllLines(TestData.resolve("docs/validation/baseline/" + job + "/" + dd + ".txt"),
                StandardCharsets.ISO_8859_1);
    }

    public static List<String> fold(Path path, int lrecl) throws IOException {
        byte[] data = Files.readAllBytes(path);
        assertThat(data.length % lrecl).isZero();
        List<String> lines = new ArrayList<>();
        for (int i = 0; i < data.length; i += lrecl) {
            lines.add(render(data, i, i + lrecl));
        }
        return lines;
    }

    public static List<String> foldVarseq0(Path path) throws IOException {
        byte[] data = Files.readAllBytes(path);
        List<String> lines = new ArrayList<>();
        for (int i = 0; i + 4 <= data.length; ) {
            int len = ((data[i] & 0xFF) << 8) | (data[i + 1] & 0xFF);
            lines.add(String.format("%05d|%s", len, render(data, i + 4, i + 4 + len)));
            i += 4 + len;
        }
        return lines;
    }
}
