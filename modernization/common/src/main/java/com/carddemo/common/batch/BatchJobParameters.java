package com.carddemo.common.batch;

import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;

import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;

import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

/**
 * The Spring Batch job parameters a launch carries, and typed accessors for steps. Every launch is
 * a new job instance ({@code run.at}), as each JCL submission is.
 */
public final class BatchJobParameters {

    public static final String JOB = "jcl.job";
    public static final String RUN_DATE = "run.date";
    public static final String PARM = "parm";
    public static final String RUN_DIR = "run.dir";
    public static final String CODE_PAGE = "codepage";
    public static final String FORMAT = "record.format";
    public static final String DD_PREFIX = "dd.";
    public static final String RUN_AT = "run.at";

    private BatchJobParameters() {
    }

    public static JobParameters of(BatchArguments args, Path runDir, Instant now) {
        JobParametersBuilder builder = new JobParametersBuilder()
                .addString(JOB, args.job())
                .addString(RUN_DIR, runDir.toAbsolutePath().toString())
                .addString(CODE_PAGE, args.codePage().name())
                .addString(FORMAT, args.format().organization().name())
                .addLong(RUN_AT, now.toEpochMilli());
        if (args.runDate() != null) {
            builder.addString(RUN_DATE, args.runDate().toString());
        }
        if (args.parm() != null) {
            builder.addString(PARM, args.parm());
        }
        args.dd().forEach((dd, path) -> builder.addString(DD_PREFIX + dd, path.toAbsolutePath().toString(), false));
        return builder.toJobParameters();
    }

    /** {@code --date} / DATEPARM, or today in the service clock when not given. */
    public static LocalDate runDate(JobParameters params, Clock clock) {
        String value = params.getString(RUN_DATE);
        return value == null ? LocalDate.now(clock) : LocalDate.parse(value);
    }

    /** {@code PARM='...'}, or {@code yyyyMMdd00} of the run date (the shape of INTCALC's {@code PARM='2022071800'}). */
    public static String parm(JobParameters params, Clock clock) {
        String value = params.getString(PARM);
        return value != null ? value : runDate(params, clock).format(DateTimeFormatter.BASIC_ISO_DATE) + "00";
    }

    public static JobRunDirectory runDirectory(JobParameters params) {
        String value = params.getString(RUN_DIR);
        if (value == null) {
            throw new IllegalStateException("Job launched without " + RUN_DIR + "; use BatchJobLauncher");
        }
        return JobRunDirectory.of(Path.of(value));
    }

    public static Optional<Path> dd(JobParameters params, String ddName) {
        return Optional.ofNullable(params.getString(DD_PREFIX + ddName)).map(Path::of);
    }

    public static CodePage codePage(JobParameters params) {
        String value = params.getString(CODE_PAGE);
        return value == null ? CodePage.ASCII : CodePage.valueOf(value);
    }

    public static RecordFormat format(JobParameters params) {
        String value = params.getString(FORMAT);
        return RecordFormat.Organization.LINE_SEQUENTIAL.name().equals(value)
                ? RecordFormat.LINE_SEQUENTIAL : RecordFormat.FIXED;
    }
}
