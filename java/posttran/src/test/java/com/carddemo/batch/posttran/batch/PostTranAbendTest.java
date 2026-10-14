package com.carddemo.batch.posttran.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.posttran.TestClockConfig;
import com.carddemo.batch.posttran.TestData;
import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.cobol.Sysout;
import com.carddemo.batch.posttran.seed.VsamDatasets;
import com.carddemo.batch.posttran.seed.VsamDatasets.Seed;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;

/** File-status failures end the job the way 9999-ABEND-PROGRAM does. */
@SpringBootTest(properties = {"posttran.run-on-startup=false", "spring.datasource.url=jdbc:h2:mem:abend-test"})
@Import(TestClockConfig.class)
class PostTranAbendTest {

    @Autowired
    private VsamDatasets datasets;
    @Autowired
    private PostTranLauncher launcher;
    @Autowired
    private Sysout sysout;

    @TempDir
    Path tmp;

    private final ByteArrayOutputStream captured = new ByteArrayOutputStream();

    @BeforeEach
    void setUp() {
        datasets.load(TestData.dataDir(), Seed.ASCII);
        sysout.redirect(new PrintStream(captured, true, StandardCharsets.UTF_8));
    }

    @AfterEach
    void restoreSysout() {
        sysout.redirect(System.out);
    }

    @Test
    void missingDalytranAbendsOnOpen() {
        PostTranResult result = launcher.run(tmp.resolve("missing"), tmp.resolve("rejs"), RecordFormat.ASCII);

        assertThat(result.returnCode()).isEqualTo(PostTranResult.RC_ABEND);
        assertThat(result.abendCause()).isInstanceOf(Abend.class);
        assertThat(captured.toString(StandardCharsets.UTF_8).lines())
                .containsSequence("ERROR OPENING DALYTRAN", "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM")
                .noneMatch(l -> l.startsWith("TRANSACTIONS PROCESSED"));
    }

    @Test
    void rerunAgainstPostedTranfileAbendsOnDuplicateKey() {
        Path dalytran = Seed.ASCII.dalytran(TestData.dataDir());
        assertThat(launcher.run(dalytran, tmp.resolve("rejs1"), RecordFormat.ASCII).returnCode()).isEqualTo(4);
        captured.reset();

        PostTranResult rerun = launcher.run(dalytran, tmp.resolve("rejs2"), RecordFormat.ASCII);

        assertThat(rerun.returnCode()).isEqualTo(PostTranResult.RC_ABEND);
        assertThat(captured.toString(StandardCharsets.UTF_8).lines())
                .containsSequence("ERROR WRITING TO TRANSACTION FILE", "FILE STATUS IS: NNNN0022", "ABENDING PROGRAM");
    }

    @Test
    void truncatedFixedRecordAbendsOnRead() throws Exception {
        Path shortFile = tmp.resolve("short.ps");
        Files.write(shortFile, new byte[349]);

        PostTranResult result = launcher.run(shortFile, tmp.resolve("rejs"), RecordFormat.EBCDIC);

        assertThat(result.returnCode()).isEqualTo(PostTranResult.RC_ABEND);
        assertThat(captured.toString(StandardCharsets.UTF_8).lines())
                .containsSequence("ERROR READING DALYTRAN FILE", "FILE STATUS IS: NNNN0004", "ABENDING PROGRAM");
    }
}
