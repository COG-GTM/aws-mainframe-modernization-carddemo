package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.io.FixedWidthFileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

/**
 * Batch entry point replacing CREASTMT STEP040 (EXEC PGM=CBSTM03A). DD names are
 * resolved from environment variables of the same name (TRNXFILE, XREFFILE, CUSTFILE,
 * ACCTFILE as line-per-record ASCII inputs; STMTFILE, HTMLFILE as outputs written one
 * record per line). Exit code 0 on GOBACK, {@value #ABEND_EXIT_CODE} on ABEND (outputs deleted,
 * as DISP=(NEW,CATLG,DELETE) does).
 */
public final class Cbstm03aMain {

    public static final int ABEND_EXIT_CODE = 16;
    private static final List<String> DD_NAMES =
            List.of("TRNXFILE", "XREFFILE", "CUSTFILE", "ACCTFILE", "STMTFILE", "HTMLFILE");

    private Cbstm03aMain() {
    }

    public static void main(String[] args) {
        System.exit(run(System.getenv()));
    }

    static int run(Map<String, String> env) {
        for (String dd : DD_NAMES) {
            if (env.get(dd) == null || env.get(dd).isBlank()) {
                System.err.println("Missing DD " + dd + " (set environment variable " + dd + ")");
                return ABEND_EXIT_CODE;
            }
        }
        JobContext job = new JobContext(env.getOrDefault("JOBNAME", "CREASTMT"), env.getOrDefault("STEPNAME", "STEP040"),
                DD_NAMES.stream().map(dd -> new JobContext.DdEntry(dd, true)).toList());
        Cbstm03bFiles files = Cbstm03bFiles.fromFiles(Path.of(env.get("TRNXFILE")), Path.of(env.get("XREFFILE")),
                Path.of(env.get("CUSTFILE")), Path.of(env.get("ACCTFILE")));
        FixedWidthFileWriter stmt = new FixedWidthFileWriter(Path.of(env.get("STMTFILE")), 80, true);
        FixedWidthFileWriter html = new FixedWidthFileWriter(Path.of(env.get("HTMLFILE")), 100, true);
        try {
            new Cbstm03a(job, files, stmt, html, System.out::println).run();
            return 0;
        } catch (Cbstm03aAbendException e) {
            // STMTFILE / HTMLFILE are DISP=(NEW,CATLG,DELETE): abnormal end deletes them.
            stmt.close();
            html.close();
            deleteQuietly(Path.of(env.get("STMTFILE")));
            deleteQuietly(Path.of(env.get("HTMLFILE")));
            return ABEND_EXIT_CODE;
        }
    }

    private static void deleteQuietly(Path path) {
        try {
            Files.deleteIfExists(path);
        } catch (IOException e) {
            System.err.println("Could not delete " + path + ": " + e.getMessage());
        }
    }
}
