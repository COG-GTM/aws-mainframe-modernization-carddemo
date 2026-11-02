package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.batch.ReturnCodes;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

/** Input errors end the job with the abend return code instead of output. */
@SpringBootTest(properties = "spring.main.allow-bean-definition-overriding=true")
class AbendTest {

    @Autowired
    private BatchJobLauncher launcher;

    @TempDir
    Path out;

    @Test
    void duplicateKsdsKeyAbends() throws Exception {
        List<String> lines = Files.readAllLines(GoldenSetTest.DATA.resolve("cardxref.txt"));
        lines.add(lines.get(0));
        Path xref = Files.write(out.resolve("cardxref.txt"), lines);
        assertThat(run("READXREF", "--dd=XREFFILE=" + xref).returnCode()).isEqualTo(ReturnCodes.ABEND);
    }

    @Test
    void moreThanTenTransactionsForOneCardAbendsTheStatementStep() throws Exception {
        List<String> daily = new ArrayList<>(Files.readAllLines(GoldenSetTest.DATA.resolve("dailytran.txt")));
        String first = daily.get(0);
        for (int i = 0; i < 11; i++) {
            daily.add(String.format("%016d", 9_000_000_000L + i) + first.substring(16));
        }
        Path transact = Files.write(out.resolve("dailytran.txt"), daily);
        BatchRun run = run("CREASTMT", "--dd=TRANSACT=" + transact,
                "--dd=XREFFILE=" + GoldenSetTest.DATA.resolve("cardxref.txt"),
                "--dd=CUSTFILE=" + GoldenSetTest.DATA.resolve("custdata.txt"),
                "--dd=ACCTFILE=" + GoldenSetTest.DATA.resolve("acctdata.txt"));
        assertThat(run.returnCode()).isEqualTo(ReturnCodes.ABEND);
        assertThat(run.outputDir().resolve("STMTFILE")).doesNotExist();
    }

    @Test
    void importWithoutExportFileAbends() throws Exception {
        assertThat(run("CBIMPORT").returnCode()).isEqualTo(ReturnCodes.ABEND);
    }

    private BatchRun run(String job, String... dds) throws Exception {
        List<String> args = new ArrayList<>(List.of("--job=" + job, "--output-dir=" + out.resolve("gen")));
        args.addAll(List.of(dds));
        return launcher.launch(BatchArguments.parse(args.toArray(String[]::new)).orElseThrow());
    }
}
