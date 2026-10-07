package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.AbendException;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.transaction.TransactionCategoryId;
import com.carddemo.transaction.TransactionCategoryRecord;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionTypeRecord;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.JobParametersBuilder;

/**
 * TRANREPT STEP10/STEP15 on the baseline's inputs ({@code docs/validation/baseline/TRANREPT}): the extract of
 * TRANSACT.BKUP equals TRANSACT.DALY, and CBTRN03C over TRANSACT.DALY with the {@code app/data/ASCII} CARDXREF,
 * TRANTYPE, TRANCATG and DATEPARM {@code 2022-01-01 2022-07-06} prints TRANREPT.txt line for line (133 bytes each)
 * and the program part of the SYSOUT.
 */
class Cbtrn03cBaselineTest {

    @TempDir
    Path dir;

    static List<FixedWidthRecord> transactions(String job, String dd) throws IOException {
        return PrintProgramsBaselineTest.baselineFile(job, dd).stream()
                .map(l -> FixedWidthRecord.fromLine(TransactionRecord.MAPPER.layout(), l, RecordEncoding.ASCII))
                .toList();
    }

    /** The baseline SYSOUT lines CBTRN03C wrote (after the runner's {@code --- STEP15 EXEC} line). */
    static List<String> programSysout() throws IOException {
        List<String> all = PrintProgramsBaselineTest.baselineSysout("TRANREPT");
        int start = all.indexOf("--- STEP15 EXEC PGM=CBTRN03C");
        assertThat(start).isNotNegative();
        return all.subList(start + 1, all.size());
    }

    private Cbtrn03c program(Path tranfile, Path cardxref, Path dateparm, BufferedSink report, Sysout sysout) {
        return new Cbtrn03c(
                KsdsInput.file(Cbtrn03c.TRANFILE, tranfile, TransactionRecord.MAPPER.layout(), RecordEncoding.ASCII),
                KeyedDataset.file(Cbtrn03c.CARDXREF, cardxref, KeyedDataset.Mode.INPUT, CardXrefRecord.MAPPER,
                        CardXrefRecord::cardNum, k -> Cbtrn03c.pad(k, 16), RecordEncoding.ASCII),
                KeyedDataset.file(Cbtrn03c.TRANTYPE, TestData.resolve("app/data/ASCII/trantype.txt"),
                        KeyedDataset.Mode.INPUT, TransactionTypeRecord.MAPPER, TransactionTypeRecord::tranTypeCd,
                        k -> Cbtrn03c.pad(k, 2), RecordEncoding.ASCII),
                KeyedDataset.file(Cbtrn03c.TRANCATG, TestData.resolve("app/data/ASCII/trancatg.txt"),
                        KeyedDataset.Mode.INPUT, TransactionCategoryRecord.MAPPER,
                        r -> new TransactionCategoryId(r.tranTypeCd(), r.tranCatCd()), Cbtrn03c::categoryKey,
                        RecordEncoding.ASCII),
                TranreptJobConfiguration.dateparm(new JobParametersBuilder()
                        .addString(Cbtrn03c.DATEPARM, dateparm.toString()).toJobParameters(), RecordEncoding.ASCII,
                        null),
                report, RecordEncoding.ASCII, sysout);
    }

    @Test
    void theExtractOfTheBackupIsTheBaselineDailyFile() throws IOException {
        List<FixedWidthRecord> extract = TranreptJobConfiguration.extract(transactions("TRANREPT", "TRANSACT.BKUP"),
                "2022-01-01", "2022-07-06", RecordEncoding.ASCII);
        assertThat(extract).extracting(FixedWidthRecord::text)
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("TRANREPT", "TRANSACT.DALY"));
        assertThat(TranreptJobConfiguration.extract(transactions("TRANREPT", "TRANSACT.BKUP"), "2022-01-01",
                "2022-07-05", RecordEncoding.ASCII)).isEmpty();
    }

    @Test
    void cbtrn03cMatchesTheBaselineReport() throws IOException {
        Path tranfile = TestData.resolve("docs/validation/baseline/TRANREPT/TRANSACT.DALY.txt");
        BufferedSink report = new BufferedSink(Cbtrn03c.TRANREPT);
        Path sysoutPath = dir.resolve("sysout.txt");
        Cbtrn03c.Result result;
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            result = program(tranfile, TestData.resolve("app/data/ASCII/cardxref.txt"),
                    TestData.resolve("docs/validation/baseline/00-DATA/DATEPARM.txt"), report, sysout).run();
        }
        List<String> baseline = PrintProgramsBaselineTest.baselineFile("TRANREPT", "TRANREPT");
        assertThat(result).isEqualTo(new Cbtrn03c.Result(312, 312, baseline.size(), ReturnCode.OK));
        assertThat(report.records()).allSatisfy(r -> assertThat(r.bytes()).hasSize(Cbtrn03c.REPORT_LRECL));
        assertThat(report.records()).extracting(FixedWidthRecord::text).containsExactlyElementsOf(baseline);
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysoutPath)).containsExactlyElementsOf(programSysout());
    }

    @Test
    void reportLayoutsAreTheCopybookLayouts() {
        assertThat(Cbtrn03c.reportNameHeader("2022-01-01", "2022-07-06")).hasSize(115);
        assertThat(Cbtrn03c.columnHeader()).hasSize(114);
        String detail = Cbtrn03c.detail("0000000000000001", 1, "01", "Purchase", "0001", "Regular Sales Draft",
                "POS TERM", new BigDecimal("-1234.5"));
        // TRANSACTION-DETAIL-REPORT is 114 bytes; the FD record (133) pads it with spaces.
        assertThat(detail).hasSize(114)
                .endsWith("    -      1,234.50  ").contains(" 00000000001 01-Purchase        0001-Regular");
    }

    @Test
    void theFirstRecordOutsideTheWindowEndsTheReportWithoutTotals() throws IOException {
        Path dateparm = dir.resolve("DATEPARM");
        Files.writeString(dateparm, "2022-01-01 2022-07-05\n");
        BufferedSink report = new BufferedSink(Cbtrn03c.TRANREPT);
        Path sysoutPath = dir.resolve("window.txt");
        Cbtrn03c.Result result;
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            result = program(TestData.resolve("docs/validation/baseline/TRANREPT/TRANSACT.DALY.txt"),
                    TestData.resolve("app/data/ASCII/cardxref.txt"), dateparm, report, sysout).run();
        }
        assertThat(result).isEqualTo(new Cbtrn03c.Result(1, 0, 0, ReturnCode.OK));
        assertThat(report.records()).isEmpty();
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysoutPath)).containsExactly(
                "START OF EXECUTION OF PROGRAM CBTRN03C", "Reporting from 2022-01-01 to 2022-07-05",
                "END OF EXECUTION OF PROGRAM CBTRN03C");
    }

    @Test
    void anUnknownCardAbends() throws IOException {
        Path xref = dir.resolve("CARDXREF");
        Files.writeString(xref, "");
        Path sysoutPath = dir.resolve("abend.txt");
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            Cbtrn03c program = program(TestData.resolve("docs/validation/baseline/TRANREPT/TRANSACT.DALY.txt"), xref,
                    TestData.resolve("docs/validation/baseline/00-DATA/DATEPARM.txt"),
                    new BufferedSink(Cbtrn03c.TRANREPT), sysout);
            assertThatThrownBy(program::run).isInstanceOf(AbendException.class);
        }
        List<String> lines = PrintProgramsBaselineTest.sysoutLines(sysoutPath);
        String card = PrintProgramsBaselineTest.baselineFile("TRANREPT", "TRANSACT.DALY").get(0).substring(262, 278);
        assertThat(lines.subList(lines.size() - 3, lines.size())).containsExactly(
                "INVALID CARD NUMBER : " + card, "FILE STATUS IS: NNNN0023", "ABENDING PROGRAM");
    }
}
