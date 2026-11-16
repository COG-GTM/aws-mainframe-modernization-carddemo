package com.carddemo.batch.jobs.creastmt;

import com.carddemo.batch.io.CardDemoData;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Runs creastmtJob on the app/data/ASCII samples and compares STMTFILE and HTMLFILE
 * byte for byte with the GnuCOBOL golden outputs of CBSTM03A/CBSTM03B
 * (regenerate with java-batch/parity/creastmt/regen-golden.sh).
 */
@SpringBootTest
class CreastmtParityTest {

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    @Qualifier("creastmtJob")
    private Job creastmtJob;

    @TempDir
    Path work;

    @Test
    void statementAndHtmlMatchCobolGoldenOutputs() throws Exception {
        Path stmt = work.resolve("STMTFILE.dat");
        Path html = work.resolve("HTMLFILE.dat");
        JobParameters params = new JobParametersBuilder()
                .addString("transactFile", CardDemoData.file(CardDemoData.DAILY_TRANSACTIONS).toString())
                .addString("trxflFile", work.resolve("TRXFL.dat").toString())
                .addString("xrefFile", CardDemoData.file(CardDemoData.CARD_XREF).toString())
                .addString("custFile", CardDemoData.file(CardDemoData.CUSTOMERS).toString())
                .addString("acctFile", CardDemoData.file(CardDemoData.ACCOUNTS).toString())
                .addString("stmtFile", stmt.toString())
                .addString("htmlFile", html.toString())
                .toJobParameters();

        JobExecution execution = jobLauncher.run(creastmtJob, params);

        assertThat(execution.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertSameBytes(stmt, "/golden/creastmt/STMTFILE.dat", 80);
        assertSameBytes(html, "/golden/creastmt/HTMLFILE.dat", 100);
    }

    private static void assertSameBytes(Path actualFile, String goldenResource, int lrecl) throws IOException {
        byte[] expected;
        try (InputStream in = CreastmtParityTest.class.getResourceAsStream(goldenResource)) {
            assertThat(in).as(goldenResource).isNotNull();
            expected = in.readAllBytes();
        }
        byte[] actual = Files.readAllBytes(actualFile);
        int mismatch = java.util.Arrays.mismatch(expected, actual);
        if (mismatch >= 0) {
            int rec = mismatch / lrecl;
            String exp = record(expected, rec, lrecl);
            String act = record(actual, rec, lrecl);
            throw new AssertionError(goldenResource + " differs at byte " + mismatch + " (record " + (rec + 1)
                    + ")\n expected: [" + exp + "]\n actual:   [" + act + "]");
        }
        assertThat(actual.length % lrecl).isZero();
    }

    private static String record(byte[] data, int rec, int lrecl) {
        int from = Math.min(rec * lrecl, data.length);
        int to = Math.min(from + lrecl, data.length);
        return new String(data, from, to - from, java.nio.charset.StandardCharsets.US_ASCII);
    }
}
