package com.carddemo.batch.pause;

import com.carddemo.batch.JclJob;
import com.carddemo.common.batch.BatchJobParameters;
import com.carddemo.common.batch.ReturnCodes;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.Duration;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * WAITSTEP (COBSWAIT): sleeps for the delay in centiseconds given as the first 8 characters of SYSIN
 * ({@code --dd=SYSIN=<file>}) or {@code --parm}. The Assembler routine {@code MVSWAIT} that COBSWAIT calls
 * (an STIMER wait) is replaced by {@link Thread#sleep}, not ported. The schedules do not need it
 * (jcl-mapping.md); it exists so the job can still be launched.
 */
@Configuration
public class WaitStepJobConfig {

    public static final String WAITSTEP = "WAITSTEP";

    @Bean
    public JclJob waitStep(JobRepository repository, PlatformTransactionManager tx) {
        Job job = new JobBuilder("waitStepJob", repository)
                .start(new StepBuilder("WAIT", repository).tasklet((contribution, chunk) -> {
                    Duration delay = delay(contribution.getStepExecution().getJobParameters());
                    Thread.sleep(delay.toMillis());
                    contribution.getStepExecution().getExecutionContext().putLong("waitedMillis", delay.toMillis());
                    ReturnCodes.set(contribution, 0);
                    return RepeatStatus.FINISHED;
                }, tx).build())
                .build();
        return new JclJob(WAITSTEP, job);
    }

    /** {@code ACCEPT PARM-VALUE FROM SYSIN} (X(8)), {@code MOVE PARM-VALUE TO MVSWAIT-TIME} (9(8) COMP): centiseconds. */
    static Duration delay(JobParameters params) {
        String parm = params.getString(BatchJobParameters.PARM);
        if (parm == null) {
            parm = BatchJobParameters.dd(params, "SYSIN").map(file -> {
                try {
                    return Files.readString(file, StandardCharsets.ISO_8859_1);
                } catch (IOException ex) {
                    throw new UncheckedIOException(ex);
                }
            }).orElseThrow(() -> new IllegalArgumentException("WAITSTEP needs --parm=<centiseconds> or --dd=SYSIN=<file>"));
        }
        String value = (parm.length() > 8 ? parm.substring(0, 8) : parm).trim();
        if (!value.matches("\\d{1,8}")) {
            throw new IllegalArgumentException("WAITSTEP delay is not numeric: '" + value + "'");
        }
        return Duration.ofMillis(Long.parseLong(value) * 10);
    }
}
