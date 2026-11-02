package com.carddemo.common.batch;

import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;

import java.nio.file.Path;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

/**
 * What a JCL job card and its DD statements supply, as command-line options:
 * <pre>
 *   --job=POSTTRAN            the JCL member name (EXEC of the job)
 *   --date=2022-07-18         the run date; DATEPARM for TRANREPT-style jobs (also 20220718)
 *   --parm=2022071800         EXEC PARM='...' of the program step; INTCALC defaults it from --date
 *   --output-dir=out          root of the generation directories ({@code <root>/<JOB>/<timestamp>/<DD>})
 *   --dd=DALYTRAN=path        a DD bound to a file (input DSN, or (0) generation override); repeatable
 *   --codepage=ascii|ebcdic   code page of fixed-width files (default ascii)
 *   --format=fixed|line       organization of written files (default fixed, as RECFM=F)
 * </pre>
 * Any other argument (e.g. {@code --spring.datasource.url=...}) is left to Spring Boot.
 */
public record BatchArguments(String job, LocalDate runDate, String parm, Path outputRoot,
                             Map<String, Path> dd, CodePage codePage, RecordFormat format) {

    public static final String JOB = "--job=";
    private static final DateTimeFormatter BASIC_DATE = DateTimeFormatter.BASIC_ISO_DATE;

    public BatchArguments {
        if (job == null || job.isBlank()) {
            throw new IllegalArgumentException("--job is required");
        }
        job = job.trim().toUpperCase(Locale.ROOT);
        dd = Collections.unmodifiableMap(new LinkedHashMap<>(dd == null ? Map.of() : dd));
        codePage = codePage == null ? CodePage.ASCII : codePage;
        format = format == null ? RecordFormat.FIXED : format;
        if (codePage == CodePage.EBCDIC && format == RecordFormat.LINE_SEQUENTIAL) {
            throw new IllegalArgumentException("--codepage=ebcdic needs --format=fixed: line sequential files are ASCII");
        }
    }

    /** Arguments for a launch that only names the job (the HTTP endpoints). */
    public static BatchArguments of(String job, LocalDate runDate, String parm) {
        return new BatchArguments(job, runDate, parm, null, Map.of(), null, null);
    }

    /** True when the process was started as a batch launch rather than as the web service. */
    public static boolean isBatchLaunch(String... args) {
        for (String arg : args) {
            if (arg.startsWith(JOB)) {
                return true;
            }
        }
        return false;
    }

    /** Parses the batch options; empty when no {@code --job} is given. */
    public static Optional<BatchArguments> parse(String... args) {
        String job = null;
        LocalDate date = null;
        String parm = null;
        Path output = null;
        Map<String, Path> dd = new LinkedHashMap<>();
        CodePage codePage = null;
        RecordFormat format = null;
        for (String arg : args) {
            if (!arg.startsWith("--") || !arg.contains("=")) {
                continue;
            }
            String name = arg.substring(2, arg.indexOf('='));
            String value = arg.substring(arg.indexOf('=') + 1);
            switch (name) {
                case "job" -> job = value;
                case "date" -> date = parseDate(value);
                case "parm" -> parm = value;
                case "output-dir" -> output = Path.of(value);
                case "dd" -> {
                    int eq = value.indexOf('=');
                    if (eq <= 0 || eq == value.length() - 1) {
                        throw new IllegalArgumentException("--dd expects DDNAME=path, got '" + value + "'");
                    }
                    dd.put(value.substring(0, eq).trim().toUpperCase(Locale.ROOT), Path.of(value.substring(eq + 1)));
                }
                case "codepage" -> codePage = switch (value.toLowerCase(Locale.ROOT)) {
                    case "ascii" -> CodePage.ASCII;
                    case "ebcdic", "ibm037", "cp037" -> CodePage.EBCDIC;
                    default -> throw new IllegalArgumentException("--codepage must be ascii or ebcdic, got '" + value + "'");
                };
                case "format" -> format = switch (value.toLowerCase(Locale.ROOT)) {
                    case "fixed" -> RecordFormat.FIXED;
                    case "line" -> RecordFormat.LINE_SEQUENTIAL;
                    default -> throw new IllegalArgumentException("--format must be fixed or line, got '" + value + "'");
                };
                default -> {
                    // not a batch option; Spring Boot binds it
                }
            }
        }
        if (job == null) {
            return Optional.empty();
        }
        return Optional.of(new BatchArguments(job, date, parm, output, dd, codePage, format));
    }

    /** {@code --date} as {@code 2022-07-18} or {@code 20220718}. */
    public static LocalDate parseDate(String value) {
        String text = value.trim();
        try {
            return text.length() == 8 ? LocalDate.parse(text, BASIC_DATE) : LocalDate.parse(text);
        } catch (DateTimeParseException ex) {
            throw new IllegalArgumentException("--date must be YYYY-MM-DD or YYYYMMDD, got '" + value + "'", ex);
        }
    }

    public Optional<Path> ddPath(String ddName) {
        return Optional.ofNullable(dd.get(ddName.toUpperCase(Locale.ROOT)));
    }

    public static String usage() {
        return "usage: java -jar <service>.jar --job=<JCL member> [--date=YYYY-MM-DD] [--parm=<PARM>]"
                + " [--output-dir=<dir>] [--dd=<DDNAME>=<path>]... [--codepage=ascii|ebcdic] [--format=fixed|line]";
    }
}
