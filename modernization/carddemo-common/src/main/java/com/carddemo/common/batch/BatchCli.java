package com.carddemo.common.batch;

import com.carddemo.common.error.AbendException;

import java.io.PrintStream;

/**
 * CLI launch seam ({@code java -jar carddemo-app.jar --job=POSTTRAN --dd=DALYTRAN=<file> --output-dir=<dir>}):
 * the process exit code is the JCL condition code. Usage errors exit {@link ReturnCode#JCL_ERROR};
 * any abend or uncaught exception exits {@link ReturnCode#ABEND}; otherwise the job's own RC.
 * Use from {@code main}: {@code System.exit(BatchCli.run(args, runner, System.err))}.
 */
public final class BatchCli {

    /** Runs one parsed launch and returns the job's condition code. */
    @FunctionalInterface
    public interface JobRunner {
        int run(BatchArguments arguments) throws Exception;
    }

    private BatchCli() {
    }

    public static boolean requested(String... args) {
        return BatchArguments.isBatchLaunch(args);
    }

    public static int run(String[] args, JobRunner runner, PrintStream err) {
        BatchArguments arguments;
        try {
            arguments = BatchArguments.parse(args);
        } catch (BatchUsageException ex) {
            err.println(ex.getMessage());
            err.println(BatchArguments.USAGE);
            return ReturnCode.JCL_ERROR;
        }
        try {
            int rc = runner.run(arguments);
            if (rc < 0 || rc > 4095) {
                err.println("Job " + arguments.job() + " returned an invalid RC " + rc + "; treated as abend");
                return ReturnCode.ABEND;
            }
            return rc;
        } catch (BatchUsageException ex) {
            err.println(ex.getMessage());
            return ReturnCode.JCL_ERROR;
        } catch (AbendException ex) {
            err.println("Job " + arguments.job() + " abended (ABCODE " + ex.abendCode() + "): " + ex.getMessage());
            return ex.returnCode();
        } catch (Exception | Error ex) {
            err.println("Job " + arguments.job() + " abended: " + ex);
            return ReturnCode.ABEND;
        }
    }
}
