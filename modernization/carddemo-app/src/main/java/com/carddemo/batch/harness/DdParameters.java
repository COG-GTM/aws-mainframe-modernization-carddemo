package com.carddemo.batch.harness;

import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.file.RecordPrefix;
import java.nio.file.Path;
import java.util.Locale;
import org.springframework.batch.core.JobParameters;

/**
 * Job parameters that replace JCL {@code DD} statements: {@code --ACCTFILE=table} (default for VSAM inputs: the
 * PostgreSQL table) or {@code --ACCTFILE=<path>} (a fixed-width file through the common codec);
 * {@code --OUTFILE=<path>} for outputs (default {@code <carddemo.batch.output-dir>/<DSN>}); {@code --SYSOUT=<path>};
 * {@code --encoding=EBCDIC|ASCII} for every file the step reads or writes (default EBCDIC, the mainframe image);
 * {@code --record-prefix=ZOS_RDW|GNUCOBOL_VARSEQ|NONE} for {@code RECFM=V} outputs (default ZOS_RDW).
 */
public final class DdParameters {

    public static final String TABLE = "table";
    public static final String ENCODING = "encoding";
    public static final String SYSOUT = "SYSOUT";
    public static final String RECORD_PREFIX = "record-prefix";

    private DdParameters() {
    }

    public static String value(JobParameters parameters, String ddname) {
        return parameters.getString(ddname);
    }

    /** True when the DD reads/writes the PostgreSQL table: absent or {@code table}. */
    public static boolean isTable(JobParameters parameters, String ddname) {
        String value = value(parameters, ddname);
        return value == null || value.isBlank() || value.equalsIgnoreCase(TABLE);
    }

    public static Path path(JobParameters parameters, String ddname) {
        if (isTable(parameters, ddname)) {
            throw new IllegalArgumentException(ddname + " is a table, not a file");
        }
        return Path.of(value(parameters, ddname));
    }

    /** An output dataset: the given path, or {@code outputDir/defaultDsn}. */
    public static Path output(JobParameters parameters, String ddname, Path outputDir, String defaultDsn) {
        return isTable(parameters, ddname) ? outputDir.resolve(defaultDsn) : Path.of(value(parameters, ddname));
    }

    /** {@code --SYSOUT}, else {@code outputDir/SYSOUT/<job>.<run-date>.<jobExecutionId>.txt}. */
    public static Path sysout(JobParameters parameters, Path outputDir, String jobName, long jobExecutionId) {
        String value = value(parameters, SYSOUT);
        if (value != null && !value.isBlank()) {
            return Path.of(value);
        }
        Object runDate = parameters.getParameters().containsKey(BatchCommandLine.RUN_DATE)
                ? parameters.getParameters().get(BatchCommandLine.RUN_DATE).getValue() : "undated";
        return outputDir.resolve(SYSOUT).resolve(jobName + "." + runDate + "." + jobExecutionId + ".txt");
    }

    public static RecordEncoding encoding(JobParameters parameters) {
        String value = parameters.getString(ENCODING);
        if (value == null || value.isBlank()) {
            return RecordEncoding.EBCDIC;
        }
        String name = value.strip().toUpperCase(Locale.ROOT);
        if (!name.equals("EBCDIC") && !name.equals("ASCII")) {
            throw new IllegalArgumentException("--encoding must be EBCDIC or ASCII, got '" + value + "'");
        }
        return RecordEncoding.of(name);
    }

    public static RecordPrefix recordPrefix(JobParameters parameters) {
        String value = parameters.getString(RECORD_PREFIX);
        if (value == null || value.isBlank()) {
            return RecordPrefix.ZOS_RDW;
        }
        try {
            return RecordPrefix.valueOf(value.strip().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("--record-prefix must be ZOS_RDW, GNUCOBOL_VARSEQ, GNUCOBOL_VARSEQ_0 or NONE, got '"
                    + value + "'", e);
        }
    }
}
