package com.carddemo.batch.harness;

import java.util.regex.Pattern;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.boot.ApplicationArguments;

/**
 * The batch CLI: {@code java -jar carddemo-app.jar --job=<name> [--run-date=YYYY-MM-DD] [--<DDNAME>=<path>|table]
 * [--<parameter>=<value>] ...}. Every option other than {@code job}, {@code run-date} and Spring/application
 * properties ({@code spring.*}, {@code carddemo.*}, {@code logging.*}, {@code server.*}, {@code management.*}) becomes
 * a string job parameter, replacing JCL symbolics and {@code DD} statements. {@code --spring.batch.job.name} is
 * accepted as an alias of {@code --job}.
 */
public final class BatchCommandLine {

    public static final String JOB = "job";
    public static final String BOOT_JOB_NAME = "spring.batch.job.name";
    public static final String RUN_DATE = "run-date";
    public static final String RUN_ID = "run.id";

    private static final List<String> RESERVED_PREFIXES =
            List.of("spring.", "carddemo.", "logging.", "server.", "management.");
    private static final Pattern SENSITIVE =
            Pattern.compile("password|passwd|secret|token|credential|api[-_.]?key", Pattern.CASE_INSENSITIVE);
    private static final List<String> RESERVED = List.of(JOB, RUN_DATE, "debug", "trace");

    /** A parsed launch request; {@code runDate} null = the business date of the injected clock. */
    public record Request(String jobName, LocalDate runDate, Map<String, String> parameters) {
    }

    private BatchCommandLine() {
    }

    /** True when the arguments ask for a batch job (the app then runs without a web server and exits with the RC). */
    public static boolean isBatchLaunch(String... args) {
        for (String arg : args) {
            if (arg.equals("--" + JOB) || arg.startsWith("--" + JOB + "=") || arg.equals("--" + BOOT_JOB_NAME)
                    || arg.startsWith("--" + BOOT_JOB_NAME + "=")) {
                return true;
            }
        }
        return false;
    }

    /** Empty when no job was requested; {@link IllegalArgumentException} when the request is malformed. */
    public static Optional<Request> parse(ApplicationArguments args) {
        String job = single(args, JOB);
        if (job == null) {
            job = single(args, BOOT_JOB_NAME);
        }
        if (job == null) {
            return Optional.empty();
        }
        if (job.isBlank()) {
            throw new IllegalArgumentException("--job needs a job name");
        }
        if (!args.getNonOptionArgs().isEmpty()) {
            throw new IllegalArgumentException("unexpected arguments " + args.getNonOptionArgs()
                    + "; job parameters are --name=value");
        }
        LocalDate runDate = null;
        String date = single(args, RUN_DATE);
        if (date != null) {
            try {
                runDate = LocalDate.parse(date);
            } catch (DateTimeParseException e) {
                throw new IllegalArgumentException("--run-date must be YYYY-MM-DD, got '" + date + "'", e);
            }
        }
        Map<String, String> parameters = new LinkedHashMap<>();
        for (String name : args.getOptionNames()) {
            if (RESERVED.contains(name) || RESERVED_PREFIXES.stream().anyMatch(name::startsWith)) {
                continue;
            }
            if (isSensitive(name)) {
                throw new IllegalArgumentException("--" + name + ": credentials are not job parameters (they would be"
                        + " stored in the job repository, batch_run and the log); pass them through the environment");
            }
            String value = single(args, name);
            parameters.put(name, value.isEmpty() ? "true" : value);
        }
        return Optional.of(new Request(job.strip(), runDate, parameters));
    }

    /** Parameter names that look like credentials; they are never accepted, logged or stored. */
    public static boolean isSensitive(String name) {
        return SENSITIVE.matcher(name).find();
    }

    /** The job named by {@code --job} or {@code --spring.batch.job.name}, without validation (for error records). */
    public static String requestedJob(ApplicationArguments args) {
        for (String name : List.of(JOB, BOOT_JOB_NAME)) {
            List<String> values = args.getOptionValues(name);
            if (values != null && !values.isEmpty() && !values.get(0).isBlank()) {
                return values.get(0).strip();
            }
        }
        return null;
    }

    private static String single(ApplicationArguments args, String name) {
        List<String> values = args.getOptionValues(name);
        if (values == null) {
            return null;
        }
        if (values.size() > 1) {
            throw new IllegalArgumentException("--" + name + " given " + values.size() + " times");
        }
        return values.isEmpty() ? "" : values.get(0);
    }
}
