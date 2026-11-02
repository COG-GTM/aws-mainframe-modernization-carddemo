package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordWriter;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

/** The whole TRANREPT job (REPROC, SORT, CBTRN03C) launched as the CLI does, with every DD a file. */
@SpringBootTest
class TranreptJobTest {

    @Autowired
    private BatchJobLauncher launcher;

    @TempDir
    Path out;

    static Stream<TranreptCases> jobCases() throws IOException {
        return TranreptCases.all().filter(c -> !c.programMode());
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("jobCases")
    void matchesTheCobolJob(TranreptCases c) throws Exception {
        BatchRun run = launcher.launch(arguments(c, c.transact(), CodePage.ASCII, RecordFormat.FIXED));

        assertThat(run.returnCode()).isEqualTo(c.expectedReturnCode());
        assertThat(Files.readString(run.outputDir().resolve("RETURN-CODE")).strip()).isEqualTo("0");
        assertThat(ascii(Files.readAllBytes(run.outputDir().resolve("TRANREPT"))))
                .isEqualTo(ascii(c.expectedReport().orElseThrow()));
        assertThat(Files.size(run.outputDir().resolve("TRANSACT.BKUP")))
                .isEqualTo(350L * c.transactions().size());
    }

    @Test
    void sortStepSelectsTheWindowByCardNumber() throws Exception {
        TranreptCases window = new TranreptCases(TranreptCases.ROOT.resolve("window"));
        BatchRun run = launcher.launch(arguments(window, window.transact(), CodePage.ASCII, RecordFormat.FIXED));

        List<CobolRecord> daly = DdFiles.read(run.outputDir().resolve("TRANSACT.DALY"), TranRecordLayout.LAYOUT,
                CodePage.ASCII);
        assertThat(daly).isNotEmpty().allSatisfy(r -> assertThat(r.get(TranRecordLayout.TRAN_PROC_TS).substring(0, 10))
                .isBetween("2022-03-01", "2022-05-31"));
        assertThat(daly).extracting(r -> r.get(TranRecordLayout.TRAN_CARD_NUM)).isSorted();
        assertThat(daly.size()).isLessThan(window.transactions().size());
        assertThat(run.summary().get("results")).asInstanceOf(org.assertj.core.api.InstanceOfAssertFactories.MAP)
                .containsEntry("TRANSACT.DALY", daly.size())
                .containsEntry("dateRange", "2022-03-01 to 2022-05-31");
    }

    @Test
    void lineSequentialOutputHoldsTheSameRecords() throws Exception {
        TranreptCases sample = new TranreptCases(TranreptCases.ROOT.resolve("sample"));
        BatchRun run = launcher.launch(arguments(sample, sample.transact(), CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL));

        assertThat(run.returnCode()).isZero();
        String expected = ascii(sample.expectedReport().orElseThrow()).replaceAll("(.{133})", "$1\n");
        assertThat(Files.readString(run.outputDir().resolve("TRANREPT.txt"), StandardCharsets.ISO_8859_1))
                .isEqualTo(expected);
    }

    @Test
    void ebcdicFilesGiveTheSameReportInEbcdic() throws Exception {
        TranreptCases sample = new TranreptCases(TranreptCases.ROOT.resolve("sample"));
        Path transact = out.resolve("TRANSACT.EBCDIC");
        RecordWriter.writeAll(transact, sample.transactions().stream()
                .map(r -> r.withCodePage(CodePage.EBCDIC)).toList(), RecordFormat.FIXED);
        Map<String, Path> dd = dd(sample, transact);
        Path dateparm = out.resolve("DATEPARM.EBCDIC");
        Files.write(dateparm, CodePage.EBCDIC.encodeText(String.format("%-80s", "2022-01-01 2022-07-06")));
        dd.put("DATEPARM", dateparm);
        Path ebcdic = Path.of("..", "..", "app", "data", "EBCDIC");
        dd.put("CARDXREF", ebcdic.resolve("AWS.M2.CARDDEMO.CARDXREF.PS"));
        dd.put("TRANTYPE", ebcdic.resolve("AWS.M2.CARDDEMO.TRANTYPE.PS"));
        dd.put("TRANCATG", ebcdic.resolve("AWS.M2.CARDDEMO.TRANCATG.PS"));

        BatchRun run = launcher.launch(new BatchArguments("TRANREPT", null, null, out, dd, CodePage.EBCDIC,
                RecordFormat.FIXED));

        assertThat(run.returnCode()).isZero();
        byte[] report = Files.readAllBytes(run.outputDir().resolve("TRANREPT"));
        assertThat(CodePage.EBCDIC.decodeText(report, 0, report.length))
                .isEqualTo(ascii(sample.expectedReport().orElseThrow()));
    }

    @Test
    void abendEndsWithRc16AndNoReport() throws Exception {
        TranreptCases bad = new TranreptCases(TranreptCases.ROOT.resolve("abend-xref"));
        BatchRun run = launcher.launch(arguments(bad, bad.transact(), CodePage.ASCII, RecordFormat.FIXED));

        assertThat(run.returnCode()).isEqualTo(16);
        assertThat(Files.readString(run.outputDir().resolve("RETURN-CODE")).strip()).isEqualTo("16");
        assertThat(run.outputDir().resolve("TRANREPT")).doesNotExist();
        assertThat(run.summary().toString()).contains("INVALID CARD NUMBER : 9999999999999999");
    }

    @Test
    void missingDateparmFailsBeforeAnyOutput() throws Exception {
        TranreptCases sample = new TranreptCases(TranreptCases.ROOT.resolve("sample"));
        Map<String, Path> dd = dd(sample, sample.transact());
        dd.remove("DATEPARM");
        BatchRun run = launcher.launch(new BatchArguments("TRANREPT", null, null, out, dd, null, null));

        assertThat(run.returnCode()).isEqualTo(16);
        assertThat(run.outputDir().resolve("TRANSACT.BKUP")).doesNotExist();
        assertThat(run.summary().toString()).contains("TRANREPT needs DATEPARM");
    }

    @Test
    void parmGivesTheDateparmRecord() throws Exception {
        TranreptCases sample = new TranreptCases(TranreptCases.ROOT.resolve("sample"));
        Map<String, Path> dd = dd(sample, sample.transact());
        dd.remove("DATEPARM");
        BatchRun run = launcher.launch(new BatchArguments("TRANREPT", null, "2022-01-01 2022-07-06", out, dd, null, null));

        assertThat(run.returnCode()).isZero();
        assertThat(ascii(Files.readAllBytes(run.outputDir().resolve("TRANREPT"))))
                .isEqualTo(ascii(sample.expectedReport().orElseThrow()));
    }

    private BatchArguments arguments(TranreptCases c, Path transact, CodePage codePage, RecordFormat format) {
        return new BatchArguments("TRANREPT", null, null, out, dd(c, transact), codePage, format);
    }

    private static Map<String, Path> dd(TranreptCases c, Path transact) {
        Map<String, Path> dd = new LinkedHashMap<>();
        dd.put("TRANSACT", transact);
        dd.put("DATEPARM", c.dateparm());
        dd.put("CARDXREF", c.reference("CARDXREF"));
        dd.put("TRANTYPE", c.reference("TRANTYPE"));
        dd.put("TRANCATG", c.reference("TRANCATG"));
        return dd;
    }

    private static String ascii(byte[] bytes) {
        return new String(bytes, StandardCharsets.ISO_8859_1);
    }
}
