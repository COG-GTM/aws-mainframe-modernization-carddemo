package com.carddemo.common.batch;

import java.io.PrintStream;

/**
 * Process boundary of a batch launch: parses the arguments, runs the job and turns every outcome into the
 * JCL condition code that becomes the exit code. Usage errors exit {@link ReturnCode#JCL_ERROR}; an uncaught
 * exception exits {@link ReturnCode#ABEND} (D-006, D-015).
 */
public final class BatchCli {

    @FunctionalInterface
    public interface Runner {
        int run(BatchArguments arguments) throws Exception;
    }

    private BatchCli() {
    }

    public static int run(String[] args, Runner runner, PrintStream err) {
        BatchArguments arguments;
        try {
            arguments = BatchArguments.parse(args);
        } catch (BatchUsageException ex) {
            err.println(ex.getMessage());
            err.println(BatchArguments.USAGE);
            return ReturnCode.JCL_ERROR;
        }
        try {
            return runner.run(arguments);
        } catch (Exception ex) {
            BatchUsageException usage = usageCause(ex);
            if (usage != null) {
                err.println(usage.getMessage());
                return ReturnCode.JCL_ERROR;
            }
            err.println("Job " + arguments.job() + " abended: " + ex);
            return ReturnCode.ABEND;
        }
    }

    private static BatchUsageException usageCause(Throwable ex) {
        for (Throwable t = ex; t != null; t = t.getCause()) {
            if (t instanceof BatchUsageException usage) {
                return usage;
            }
        }
        return null;
    }
}
