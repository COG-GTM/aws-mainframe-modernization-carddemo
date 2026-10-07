package com.carddemo.batch.creastmt;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.TestData;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * CREASTMT on the baseline's start state ({@code docs/validation/baseline}, run order ... TRANREPT -> CREASTMT):
 * STEP010 over the COMBTRAN TRANSACT after-image is TRXFL.SEQ.txt record for record, and CBSTM03A over it with the
 * XREFFILE / CUSTFILE / INTCALC after-images writes STMTFILE.txt and HTMLFILE.txt byte for byte (80 / 100 bytes).
 */
class CreastmtBaselineTest {

    @TempDir
    Path dir;

    static List<FixedWidthRecord> transact() {
        return Dataset.TRANSACT.read(TestData.resolve("docs/validation/baseline/COMBTRAN/TRANSACT.ksds.txt"),
                CreastmtTestSupport.ASCII);
    }

    @Test
    void sortAndOutrecBuildTheBaselineTrxfl() throws IOException {
        List<FixedWidthRecord> trxfl = CreastmtJobConfiguration.trxfl(transact(), CreastmtTestSupport.ASCII);
        assertThat(trxfl).allSatisfy(r -> assertThat(r.length()).isEqualTo(CreastmtJobConfiguration.TRXFL_LRECL));
        assertThat(trxfl).extracting(FixedWidthRecord::text)
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "TRXFL.SEQ"));
        assertThat(CreastmtJobConfiguration.keyError(trxfl)).isNull();
    }

    @Test
    void reproRefusesDuplicateAndUnorderedKeys() {
        List<FixedWidthRecord> trxfl = CreastmtJobConfiguration.trxfl(transact(), CreastmtTestSupport.ASCII);
        List<FixedWidthRecord> duplicate = new ArrayList<>(trxfl);
        duplicate.add(1, trxfl.get(0));
        assertThat(CreastmtJobConfiguration.keyError(duplicate)).startsWith("IDC3316I DUPLICATE RECORD - KEY "
                + trxfl.get(0).text().substring(0, 32));
        List<FixedWidthRecord> unordered = new ArrayList<>(trxfl);
        unordered.add(0, trxfl.get(5));
        assertThat(CreastmtJobConfiguration.keyError(unordered)).startsWith("IDC3314I RECORD OUT OF SEQUENCE");
    }

    @Test
    void cbstm03aWritesTheBaselineStatements() throws IOException {
        List<FixedWidthRecord> trxfl = CreastmtJobConfiguration.trxfl(transact(), CreastmtTestSupport.ASCII);
        Path sysoutPath = dir.resolve("sysout.txt");
        CreastmtTestSupport.Run run;
        Cbstm03a.Result result;
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            run = CreastmtTestSupport.program(() -> trxfl, CreastmtTestSupport.XREFDATA, sysout);
            result = run.program().run();
        }
        assertThat(result.returnCode()).isEqualTo(ReturnCode.OK);
        assertThat(result.read()).isEqualTo(312);
        assertThat(result.statements()).isEqualTo(50);
        assertThat(result.transactions()).isEqualTo(312);
        assertThat(run.stmtLines()).allSatisfy(l -> assertThat(l).hasSize(Cbstm03a.STMT_LRECL))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "STMTFILE"));
        assertThat(run.htmlLines()).allSatisfy(l -> assertThat(l).hasSize(Cbstm03a.HTML_LRECL))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("CREASTMT", "HTMLFILE"));
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysoutPath))
                .containsExactly("Running JCL : CREASTMT Step STEP040");
    }
}
