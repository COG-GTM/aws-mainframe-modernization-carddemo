package com.carddemo.programs.cbtrn03c;

import com.carddemo.programs.cbtrn03c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranCatRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranTypeRecord;
import com.carddemo.programs.cbtrn03c.io.FixedWidthKeyedInputFile;
import com.carddemo.programs.cbtrn03c.io.FixedWidthSequentialInputFile;
import com.carddemo.programs.cbtrn03c.io.FixedWidthSequentialOutputFile;
import com.carddemo.programs.cbtrn03c.io.RecordFormat;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.function.Function;

/**
 * Command-line entry point standing in for JCL step TRANREPT.STEP10R.
 *
 * <p>Each DD name (TRANFILE, CARDXREF, TRANTYPE, TRANCATG, DATEPARM, TRANREPT) is resolved from the
 * system property {@code dd.NAME} or the environment variable {@code DD_NAME}. Set {@code dd.RECFM}
 * / {@code DD_RECFM} to {@code FIXED} for undelimited RECFM=FB data sets (default {@code LINE}).
 * {@link #execute} returns 0 on success and the abend code (999) on ABEND; POSIX shells see the process
 * status modulo 256 (999 -> 231). If this run opened TRANREPT, it is deleted when the step fails
 * (ABEND, or a data exception such as a non-numeric TRAN-AMT, which is rethrown), mirroring
 * {@code DISP=(NEW,CATLG,DELETE)}.
 */
public final class Cbtrn03cMain {

    private Cbtrn03cMain() {
    }

    public static void main(String[] args) {
        System.exit(execute(name -> resolve(name, System.getenv()), System.out));
    }

    static int execute(Function<String, String> dd, PrintStream sysout) {
        String recfmName = dd.apply("RECFM");
        RecordFormat recfm = recfmName == null ? RecordFormat.LINE : RecordFormat.valueOf(recfmName);
        Path report = Path.of(required(dd, "TRANREPT"));
        FixedWidthSequentialOutputFile reportFile = new FixedWidthSequentialOutputFile(report, Cbtrn03c.TRANREPT_LRECL, recfm);
        Cbtrn03c program = new Cbtrn03c(
                new FixedWidthSequentialInputFile(Path.of(required(dd, "TRANFILE")), Cbtrn03c.TRANFILE_LRECL, recfm),
                new FixedWidthKeyedInputFile(Path.of(required(dd, "CARDXREF")), CardXrefRecord.LENGTH,
                        CardXrefRecord.KEY_OFFSET, CardXrefRecord.KEY_LENGTH, recfm),
                new FixedWidthKeyedInputFile(Path.of(required(dd, "TRANTYPE")), TranTypeRecord.LENGTH,
                        TranTypeRecord.KEY_OFFSET, TranTypeRecord.KEY_LENGTH, recfm),
                new FixedWidthKeyedInputFile(Path.of(required(dd, "TRANCATG")), TranCatRecord.LENGTH,
                        TranCatRecord.KEY_OFFSET, TranCatRecord.KEY_LENGTH, recfm),
                reportFile,
                new FixedWidthSequentialInputFile(Path.of(required(dd, "DATEPARM")), Cbtrn03c.DATEPARM_LRECL, recfm),
                sysout);
        try {
            return program.run();
        } catch (AbendException e) {
            sysout.flush();
            System.err.println(e.getMessage());
            deleteIfCreated(reportFile, report);
            return e.abendCode();
        } catch (RuntimeException e) {
            sysout.flush();
            deleteIfCreated(reportFile, report);
            throw e;
        }
    }

    private static void deleteIfCreated(FixedWidthSequentialOutputFile reportFile, Path report) {
        if (!reportFile.wasOpened()) {
            return;
        }
        reportFile.close();
        try {
            Files.deleteIfExists(report);
        } catch (IOException ignored) {
            // best effort, as with DISP=(,,DELETE) on an abended step
        }
    }

    static String resolve(String ddName, Map<String, String> env) {
        String value = System.getProperty("dd." + ddName);
        return value != null ? value : env.get("DD_" + ddName);
    }

    private static String required(Function<String, String> dd, String ddName) {
        String value = dd.apply(ddName);
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("DD " + ddName + " not allocated (set -Ddd." + ddName
                    + "=<path> or DD_" + ddName + ")");
        }
        return value;
    }
}
