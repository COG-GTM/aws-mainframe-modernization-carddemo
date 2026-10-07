package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.support.Samples;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
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
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * {@code --job=unload} of the online-maintained datasets the golden set exports (scripts/golden-set): after
 * {@code initial-load}, the EBCDIC unload holds exactly the records of the {@code app/data/EBCDIC} sample, the ASCII
 * unload one record per line, both in ascending key order.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.clock.fixed=2022-07-06T00:00:00",
        "carddemo.batch.output-dir=target/unload-it/batch-output"})
@Testcontainers
class UnloadJobIT {

    static final Map<Dataset, Integer> LRECL = Map.of(Dataset.ACCTDATA, 300, Dataset.CUSTDATA, 500,
            Dataset.CARDDATA, 150, Dataset.CARDXREF, 50, Dataset.USRSEC, 80);
    static final Map<Dataset, Integer> KEY_LENGTH = Map.of(Dataset.ACCTDATA, 11, Dataset.CUSTDATA, 9,
            Dataset.CARDDATA, 16, Dataset.CARDXREF, 16, Dataset.USRSEC, 8);

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    JobLauncher jobLauncher;
    @Autowired
    @Qualifier("initialLoadJob")
    Job initialLoadJob;
    @Autowired
    @Qualifier("unloadJob")
    Job unloadJob;

    @TempDir
    Path dir;

    @Test
    void unloadReturnsTheLoadedSamplesInKeyOrder() throws Exception {
        run(initialLoadJob, InitialLoadJobConfiguration.parameters(TestData.resolve("app/data/EBCDIC"),
                LoadMode.REPLACE));
        long runId = 0;
        for (Dataset ds : LRECL.keySet().stream().sorted().toList()) {
            int lrecl = LRECL.get(ds);
            Path ebcdic = dir.resolve(ds + ".ebcdic");
            run(unloadJob, parameters(ds, ebcdic, "EBCDIC", ++runId));
            List<String> unloaded = chunks(Files.readAllBytes(ebcdic), lrecl);
            List<String> sample = chunks(Files.readAllBytes(Samples.path(ds, RecordEncoding.EBCDIC)), lrecl);
            assertThat(unloaded).as(ds + " EBCDIC unload vs sample").containsExactlyInAnyOrderElementsOf(sample);

            Path ascii = dir.resolve(ds + ".txt");
            run(unloadJob, parameters(ds, ascii, "ASCII", ++runId));
            List<String> lines = Files.readAllLines(ascii, StandardCharsets.ISO_8859_1);
            assertThat(lines).as(ds + " ASCII unload").hasSize(sample.size()).allMatch(l -> l.length() == lrecl);
            List<String> keys = new ArrayList<>(lines.stream().map(l -> l.substring(0, KEY_LENGTH.get(ds))).toList());
            String[] sorted = keys.toArray(String[]::new);
            Arrays.sort(sorted);
            assertThat(keys).as(ds + " key order").containsExactly(sorted);
        }
    }

    private static JobParameters parameters(Dataset ds, Path out, String encoding, long runId) {
        return new JobParametersBuilder().addString(UnloadJobConfiguration.DATASET, ds.name())
                .addString(UnloadJobConfiguration.OUTFILE, out.toString())
                .addString(DdParameters.ENCODING, encoding).addLong("run.id", runId).toJobParameters();
    }

    private static List<String> chunks(byte[] bytes, int lrecl) {
        assertThat(bytes.length % lrecl).isZero();
        List<String> out = new ArrayList<>();
        for (int i = 0; i < bytes.length; i += lrecl) {
            out.add(HexFormat.of().formatHex(bytes, i, i + lrecl));
        }
        return out;
    }

    private JobExecution run(Job job, JobParameters parameters) throws Exception {
        JobExecution execution = jobLauncher.run(job, parameters);
        assertThat(execution.getStatus()).as(job.getName() + " " + execution.getAllFailureExceptions())
                .isEqualTo(BatchStatus.COMPLETED);
        return execution;
    }
}
