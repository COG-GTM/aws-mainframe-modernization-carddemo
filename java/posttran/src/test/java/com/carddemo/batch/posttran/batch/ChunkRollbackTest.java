package com.carddemo.batch.posttran.batch;

import static com.carddemo.batch.posttran.TestData.dalytran;
import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.posttran.TestClockConfig;
import com.carddemo.batch.posttran.TestData;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.seed.VsamDatasets;
import com.carddemo.batch.posttran.seed.VsamDatasets.Seed;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import javax.sql.DataSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;

@SpringBootTest(properties = {"posttran.run-on-startup=false", "posttran.chunk-size=2",
        "spring.datasource.url=jdbc:h2:mem:chunk-test"})
@Import(TestClockConfig.class)
class ChunkRollbackTest {

    @Autowired
    private VsamDatasets datasets;
    @Autowired
    private PostTranLauncher launcher;
    @Autowired
    private DataSource dataSource;

    @TempDir
    Path tmp;

    @Test
    void rejectsOfARolledBackChunkAreNotWritten() throws Exception {
        datasets.load(TestData.dataDir(), Seed.ASCII);
        Path seedTran = Seed.ASCII.dalytran(TestData.dataDir());
        assertThat(launcher.run(seedTran, tmp.resolve("rejs1"), RecordFormat.ASCII).returnCode()).isEqualTo(4);
        String postedId = datasets.unloadTransactions().getFirst().substring(0, 16);
        String postedCard = datasets.unloadTransactions().getFirst().substring(262, 278);

        Path input = tmp.resolve("dalytran.txt");
        Files.write(input, List.of(
                dalytran("NEWTRAN000000001", "01", 1, "1.00", "9999999999999999", "2020-01-01 00:00:00.000000").raw(),
                dalytran(postedId, "01", 1, "1.00", postedCard, "2020-01-01 00:00:00.000000").raw()),
                StandardCharsets.ISO_8859_1);
        Path rejects = tmp.resolve("rejs2");

        PostTranResult result = launcher.run(input, rejects, RecordFormat.ASCII);

        assertThat(result.returnCode()).isEqualTo(PostTranResult.RC_ABEND);
        assertThat(Files.size(rejects)).isZero();
    }

    @Test
    void jobRepositoryNeedsNoBatchMetadataTables() {
        Integer batchTables = new JdbcTemplate(dataSource).queryForObject(
                "select count(*) from information_schema.tables where upper(table_name) like 'BATCH_%'", Integer.class);
        assertThat(batchTables).isZero();
    }
}
