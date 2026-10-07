package com.carddemo.batch.load;

import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.ReturnCodeException;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.RecordEncoding;
import java.util.Arrays;
import java.util.Locale;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * {@code repro}: IDCAMS {@code REPRO INFILE(...) OUTFILE(<KSDS>) REPLACE} of a sequential KSDS unload into its table,
 * the inverse of {@code unload}: {@code --job=repro --DATASET=<name> --INFILE=<path> [--encoding=ASCII|EBCDIC]
 * [--mode=UPSERT|REPLACE]}. {@code UPSERT} (default) replaces the rows whose key is in the file and keeps the others;
 * {@code REPLACE} empties the table first. Used to start a job from a dataset state produced elsewhere, e.g. INTCALC
 * from the POSTTRAN after-images of the GnuCOBOL baseline. Any reject ends the job with RC 12 and loads nothing.
 */
@Configuration(proxyBeanMethods = false)
public class ReproJobConfiguration {

    private static final Logger log = LoggerFactory.getLogger(ReproJobConfiguration.class);

    public static final String REPRO = "repro";
    public static final String DATASET = "DATASET";
    public static final String INFILE = "INFILE";
    public static final String MODE = "mode";

    @Bean
    Job reproJob(JobRepository jobRepository, PlatformTransactionManager transactionManager,
                 VsamDatasetLoader loader) {
        return new JobBuilder(REPRO, jobRepository)
                .start(new StepBuilder("STEP05", jobRepository).tasklet((contribution, chunk) -> {
                    StepExecution step = chunk.getStepContext().getStepExecution();
                    JobParameters parameters = step.getJobParameters();
                    RecordEncoding encoding = DdParameters.encoding(parameters);
                    String name = parameters.getString(DATASET, "").toUpperCase(Locale.ROOT);
                    Dataset dataset = Arrays.stream(Dataset.values()).filter(d -> d.name().equals(name))
                            .findFirst().orElseThrow(() -> new ReturnCodeException(ReturnCode.TERMINAL,
                                    "--DATASET must be one of " + Arrays.toString(Dataset.values()) + ", got '"
                                            + name + "'"));
                    if (DdParameters.isTable(parameters, INFILE)) {
                        throw new ReturnCodeException(ReturnCode.TERMINAL, "--INFILE=<path> is required");
                    }
                    LoadMode mode = LoadMode.valueOf(
                            parameters.getString(MODE, LoadMode.UPSERT.name()).toUpperCase(Locale.ROOT));
                    LoadResult result = loader.load(dataset, DdParameters.path(parameters, INFILE), encoding, mode);
                    if (!result.rejects().isEmpty()) {
                        LoadResult.Reject first = result.rejects().get(0);
                        throw new ReturnCodeException(ReturnCode.SEVERE, dataset + " record " + first.recordNumber()
                                + ": " + first.reason() + " (" + result.rejects().size() + " rejects)");
                    }
                    log.info("repro {} {} from {}: {} read, {} loaded, {} empty", dataset, mode,
                            parameters.getString(INFILE), result.read(), result.loaded(), result.empty());
                    for (int i = 0; i < result.read(); i++) {
                        contribution.incrementReadCount();
                    }
                    contribution.incrementWriteCount(result.loaded());
                    ReturnCode.set(step, ReturnCode.OK);
                    return RepeatStatus.FINISHED;
                }, transactionManager).build())
                .build();
    }
}
