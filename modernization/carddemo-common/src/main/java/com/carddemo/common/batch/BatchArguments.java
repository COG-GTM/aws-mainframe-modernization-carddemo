package com.carddemo.common.batch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Parsed CLI launch (D-014): {@code --job=<JCL member> [--dd=<DDNAME>=<path>]... [--run-id=<id>]}.
 * Names follow z/OS rules (1-8 chars, letters, digits, national characters) and are upper-cased.
 * Every other argument (e.g. {@code --spring.datasource.url=...}) is passed through to Spring Boot.
 */
public record BatchArguments(String job, Map<String, String> dds, String runId, List<String> passThrough) {

    public static final String USAGE =
            "usage: java -jar carddemo-app.jar --job=<JCL member> [--dd=<DDNAME>=<path>]... [--run-id=<id>]";

    private static final String JOB = "--job=";
    private static final String DD = "--dd=";
    private static final String RUN_ID = "--run-id=";
    private static final Pattern NAME = Pattern.compile("[A-Z#@$][A-Z0-9#@$]{0,7}");
    private static final Pattern RUN_ID_VALUE = Pattern.compile("[A-Za-z0-9._-]{1,64}");

    public BatchArguments {
        dds = Collections.unmodifiableMap(new LinkedHashMap<>(dds));
        passThrough = List.copyOf(passThrough);
    }

    /** True when the arguments ask for a batch launch rather than a plain application start. */
    public static boolean isBatchLaunch(String... args) {
        for (String arg : args) {
            if (arg.startsWith(JOB)) {
                return true;
            }
        }
        return false;
    }

    public static BatchArguments parse(String... args) {
        String job = null;
        String runId = null;
        Map<String, String> dds = new LinkedHashMap<>();
        List<String> passThrough = new ArrayList<>();
        for (String arg : args) {
            if (arg.startsWith(JOB)) {
                if (job != null) {
                    throw new BatchUsageException("--job given more than once");
                }
                job = name(arg.substring(JOB.length()), "job");
            } else if (arg.startsWith(DD)) {
                String spec = arg.substring(DD.length());
                int eq = spec.indexOf('=');
                if (eq <= 0 || eq == spec.length() - 1) {
                    throw new BatchUsageException("--dd must be --dd=<DDNAME>=<path>: " + arg);
                }
                String ddName = name(spec.substring(0, eq), "DD");
                if (dds.putIfAbsent(ddName, spec.substring(eq + 1)) != null) {
                    throw new BatchUsageException("DD " + ddName + " given more than once");
                }
            } else if (arg.startsWith(RUN_ID)) {
                runId = arg.substring(RUN_ID.length());
                if (!RUN_ID_VALUE.matcher(runId).matches()) {
                    throw new BatchUsageException("--run-id must match " + RUN_ID_VALUE.pattern() + ": " + runId);
                }
            } else {
                passThrough.add(arg);
            }
        }
        if (job == null) {
            throw new BatchUsageException("--job=<JCL member> is required");
        }
        return new BatchArguments(job, dds, runId, passThrough);
    }

    private static String name(String raw, String what) {
        String upper = raw.toUpperCase(Locale.ROOT);
        if (!NAME.matcher(upper).matches()) {
            throw new BatchUsageException("Invalid " + what + " name '" + raw + "' (1-8 chars, letters, digits, #@$)");
        }
        return upper;
    }
}
