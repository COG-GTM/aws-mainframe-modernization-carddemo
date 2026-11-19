package com.carddemo.common.batch;

import com.carddemo.common.cobol.CodePage;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Parsed CLI launch: {@code --job=<JCL member> [--dd=<DDNAME>=<file>]... [--dd-encoding=ASCII|CP037]
 * [--output-dir=<dir>]}. Every other argument (e.g. {@code --spring.profiles.active=local}) is kept
 * in {@link #passThrough()} for the application. Errors are {@link BatchUsageException} (RC 12).
 */
public record BatchArguments(String job, Map<String, Path> dds, CodePage ddEncoding, Optional<Path> outputDir,
                             List<String> passThrough) {

    public static final String USAGE =
            "usage: --job=<NAME> [--dd=<DDNAME>=<file>]... [--dd-encoding=ASCII|CP037] [--output-dir=<dir>]";

    public BatchArguments {
        dds = Collections.unmodifiableMap(new LinkedHashMap<>(dds));
        passThrough = List.copyOf(passThrough);
    }

    /** True when the arguments ask for a batch launch rather than a plain application start. */
    public static boolean isBatchLaunch(String... args) {
        for (String arg : args) {
            if (arg.equals("--job") || arg.startsWith("--job=")) {
                return true;
            }
        }
        return false;
    }

    public static BatchArguments parse(String... args) {
        String job = null;
        Map<String, Path> dds = new LinkedHashMap<>();
        CodePage encoding = CodePage.ASCII;
        boolean encodingSet = false;
        Path outputDir = null;
        List<String> passThrough = new ArrayList<>();
        for (String arg : args) {
            if (arg.startsWith("--job=")) {
                if (job != null) {
                    throw new BatchUsageException("--job given twice");
                }
                job = arg.substring("--job=".length()).trim().toUpperCase(java.util.Locale.ROOT);
            } else if (arg.equals("--job")) {
                throw new BatchUsageException("--job needs a value: --job=<NAME>");
            } else if (arg.startsWith("--dd=")) {
                String spec = arg.substring("--dd=".length());
                int eq = spec.indexOf('=');
                if (eq <= 0 || eq == spec.length() - 1) {
                    throw new BatchUsageException("--dd must be --dd=<DDNAME>=<file>: " + arg);
                }
                String dd = spec.substring(0, eq).trim().toUpperCase(java.util.Locale.ROOT);
                if (dds.putIfAbsent(dd, Path.of(spec.substring(eq + 1))) != null) {
                    throw new BatchUsageException("DD " + dd + " given twice");
                }
            } else if (arg.startsWith("--dd-encoding=")) {
                if (encodingSet) {
                    throw new BatchUsageException("--dd-encoding given twice");
                }
                try {
                    encoding = CodePage.parse(arg.substring("--dd-encoding=".length()));
                } catch (IllegalArgumentException ex) {
                    throw new BatchUsageException(ex.getMessage());
                }
                encodingSet = true;
            } else if (arg.startsWith("--output-dir=")) {
                String value = arg.substring("--output-dir=".length());
                if (value.isBlank()) {
                    throw new BatchUsageException("--output-dir needs a value");
                }
                outputDir = Path.of(value);
            } else {
                passThrough.add(arg);
            }
        }
        if (job == null || job.isEmpty()) {
            throw new BatchUsageException("--job is required");
        }
        if (!job.matches("[A-Z0-9@#$]{1,8}")) {
            throw new BatchUsageException("Job name must be a 1-8 character JCL member name: " + job);
        }
        return new BatchArguments(job, dds, encoding, Optional.ofNullable(outputDir), passThrough);
    }

    /** The file of a required DD (usage error when missing). */
    public Path dd(String ddName) {
        Path file = dds.get(ddName);
        if (file == null) {
            throw new BatchUsageException("Job " + job + " needs --dd=" + ddName + "=<file>");
        }
        return file;
    }
}
