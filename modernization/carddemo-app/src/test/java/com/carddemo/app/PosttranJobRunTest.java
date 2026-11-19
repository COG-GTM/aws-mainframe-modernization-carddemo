package com.carddemo.app;

import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.ParityHarness;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.RecordFiles;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

/** Restart, rerun and launch-error behaviour of {@code POSTTRAN} (FR-POSTTRAN-10/11/13/17, NFR-01, PR-CBTRN02C-01/02/14). */
@SpringBootTest
class PosttranJobRunTest extends PosttranJobSupport {

    static final int DALYREJS_LRECL = 430;

    @Autowired
    JdbcTemplate jdbc;

    @AfterEach
    void dropInjectedFailure() {
        jdbc.execute("DROP TRIGGER IF EXISTS injected_failure ON transaction.transactions");
        jdbc.execute("DROP FUNCTION IF EXISTS transaction.injected_failure()");
    }

    /** The 150th DALYTRAN record of the baseline that the oracle posted (so the failure hits mid-run, after rejects). */
    static String postedTranIdAtPosition150() {
        GoldenCase c = GoldenCase.named("baseline");
        Set<String> posted = new HashSet<>();
        for (String r : RecordFiles.readLines(c.expected("TRANSACT.unl"), 350, CobolCharsets.ASCII)) {
            posted.add(r.substring(0, 16));
        }
        List<String> input = RecordFiles.readFixed(c.dalytran(), 350, CobolCharsets.ASCII);
        return input.stream().skip(149).map(r -> r.substring(0, 16)).filter(posted::contains).findFirst().orElseThrow();
    }

    @Test
    void NFR_01_restartAfterAMidRunFailureResumesWithoutDoublePostingOrDuplicateRejects(@TempDir Path out) throws IOException {
        GoldenCase c = GoldenCase.named("baseline");
        seed(c);
        String failingId = postedTranIdAtPosition150();
        jdbc.execute("CREATE FUNCTION transaction.injected_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN "
                + "IF NEW.tran_id = '" + failingId.stripTrailing() + "' THEN RAISE EXCEPTION 'injected failure'; END IF; "
                + "RETURN NEW; END $$");
        jdbc.execute("CREATE TRIGGER injected_failure BEFORE INSERT ON transaction.transactions FOR EACH ROW "
                + "EXECUTE FUNCTION transaction.injected_failure()");

        assertThat(launch(c, out, "RESTART-1")).isEqualTo(ReturnCode.ABEND);
        int postedBeforeFailure = data.unload(Ksds.TRANSACT).size();
        long rejectBytes = Files.size(out.resolve("DALYREJS"));
        assertThat(postedBeforeFailure).isBetween(100, 261);
        assertThat(rejectBytes).isPositive().isLessThan(Files.size(c.expected("DALYREJS"))).isEqualTo(rejectBytes / DALYREJS_LRECL * DALYREJS_LRECL);
        assertThat(sysout(out)).endsWith("ERROR WRITING TO TRANSACTION FILE", "FILE STATUS IS: NNNN0030", "ABENDING PROGRAM");

        dropInjectedFailure();
        assertThat(launch(c, out, "RESTART-1")).isEqualTo(ReturnCode.WARNING);
        ParityHarness.assertDalyrejs(c, out.resolve("DALYREJS"));
        assertTables(c);
        assertThat(sysout(out)).endsWith("TRANSACTIONS PROCESSED :000000300", "TRANSACTIONS REJECTED  :000000038",
                "END OF EXECUTION OF PROGRAM CBTRN02C");
        assertThat(sysout(out)).filteredOn("START OF EXECUTION OF PROGRAM CBTRN02C"::equals).hasSize(2);
    }

    @Test
    void FR_POSTTRAN_11_aNewInstanceEmptiesTheTransactionMasterAndRepostsBalancesLikeOpenOutput(@TempDir Path out)
            throws IOException {
        GoldenCase c = GoldenCase.named("baseline");
        seed(c);
        Path first = Files.createDirectory(out.resolve("first"));
        Path second = Files.createDirectory(out.resolve("second"));
        assertThat(launch(c, first, "RERUN-1")).isEqualTo(ReturnCode.WARNING);
        List<String> accountsAfterFirst = data.unload(Ksds.ACCTDATA);
        // same DALYTRAN, new run id, DALYREJS defaulted to <output-dir>/POSTTRAN/<run.id>/DALYREJS (D-016)
        assertThat(launch(c, c.dalytran(), second, "RERUN-2", "--carddemo.batch.output-dir=" + second)).isEqualTo(ReturnCode.WARNING);
        // no duplicate-key abend: the first run's 262 postings were removed before the second run posted (D-024).
        // Balances were posted twice, so more records go over limit and fewer are posted the second time.
        List<String> secondSysout = sysout(second);
        long processed = Long.parseLong(secondSysout.get(secondSysout.size() - 3).substring(24));
        long rejected = Long.parseLong(secondSysout.get(secondSysout.size() - 2).substring(24));
        assertThat(processed).isEqualTo(300);
        assertThat(rejected).isGreaterThan(38);
        assertThat(data.unload(Ksds.TRANSACT)).hasSize((int) (processed - rejected));
        assertThat(data.unload(Ksds.ACCTDATA)).isNotEqualTo(accountsAfterFirst);
        assertThat(Files.size(second.resolve("POSTTRAN/RERUN-2/DALYREJS"))).isEqualTo(rejected * DALYREJS_LRECL);
    }

    @Test
    void FR_POSTTRAN_17_anExistingDalyrejsIsAJclErrorAndNothingRuns(@TempDir Path out) throws IOException {
        GoldenCase c = GoldenCase.named("baseline");
        seed(c);
        Files.writeString(out.resolve("DALYREJS"), "previous generation");
        assertThat(launch(c, out, "EXISTS-1")).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(Files.readString(out.resolve("DALYREJS"))).isEqualTo("previous generation");
        assertThat(data.unload(Ksds.TRANSACT)).singleElement().matches(r -> r.startsWith(STALE_TRAN_ID));
        assertThat(Files.exists(out.resolve("SYSOUT"))).isFalse();
    }

    @Test
    void missingDalytranDdIsAJclError(@TempDir Path out) {
        GoldenCase c = GoldenCase.named("empty");
        seed(c);
        String[] args = java.util.stream.Stream.concat(java.util.stream.Stream.of("--job=POSTTRAN", "--run-id=NO-DD"),
                java.util.stream.Stream.of(datasourceArgs())).toArray(String[]::new);
        assertThat(CardDemoApplication.launch(args, new java.io.PrintStream(new java.io.ByteArrayOutputStream()))).isEqualTo(ReturnCode.JCL_ERROR);
    }

    @Test
    void PR_CBTRN02C_01_missingDalytranFileAbendsBeforeTheTransactionMasterIsOpened(@TempDir Path out) {
        GoldenCase c = GoldenCase.named("empty");
        seed(c);
        assertThat(launch(c, out.resolve("NO-SUCH-FILE"), out, "NO-FILE-1", "--dd=DALYREJS=" + out.resolve("DALYREJS")))
                .isEqualTo(ReturnCode.ABEND);
        assertThat(sysout(out)).containsExactly("START OF EXECUTION OF PROGRAM CBTRN02C", "ERROR OPENING DALYTRAN",
                "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM");
        assertThat(data.unload(Ksds.TRANSACT)).singleElement().matches(r -> r.startsWith(STALE_TRAN_ID));
        assertThat(Files.exists(out.resolve("DALYREJS"))).isFalse();
    }

    @Test
    void PR_CBTRN02C_02_aShortLastRecordIsAReadErrorAfterTheCommittedRecords(@TempDir Path out) throws IOException {
        GoldenCase c = GoldenCase.named("syn-ok");
        seed(c);
        byte[] input = Files.readAllBytes(c.dalytran());
        Path dalytran = out.resolve("DALYTRAN");
        Files.write(dalytran, java.util.Arrays.copyOf(input, input.length + 100));
        assertThat(launch(c, dalytran, out, "SHORT-1", "--dd=DALYREJS=" + out.resolve("DALYREJS"))).isEqualTo(ReturnCode.ABEND);
        assertThat(sysout(out)).endsWith("ERROR READING DALYTRAN FILE", "FILE STATUS IS: NNNN0004", "ABENDING PROGRAM");
        assertThat(data.unload(Ksds.TRANSACT)).hasSize(input.length / 350);
    }
}
