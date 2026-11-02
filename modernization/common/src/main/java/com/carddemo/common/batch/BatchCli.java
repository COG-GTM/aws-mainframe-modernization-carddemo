package com.carddemo.common.batch;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.boot.WebApplicationType;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * Command-line launch of a service's jobs ({@code java -jar transaction-service.jar --job=POSTTRAN
 * --date=2022-07-18}). The service starts without its web server, runs the job once and exits with
 * the job's condition code: the highest step RC, {@link ReturnCodes#ABEND} when a step failed, or
 * {@link ReturnCodes#JCL_ERROR} when the arguments or job name are wrong.
 */
public final class BatchCli {

    private BatchCli() {
    }

    /** Use in {@code main}: {@code if (BatchCli.requested(args)) System.exit(BatchCli.run(App.class, args));} */
    public static boolean requested(String... args) {
        return BatchArguments.isBatchLaunch(args);
    }

    public static int run(Class<?> application, String... args) {
        BatchArguments arguments;
        try {
            arguments = BatchArguments.parse(args).orElseThrow(() -> new IllegalArgumentException("--job is required"));
        } catch (IllegalArgumentException ex) {
            System.err.println(ex.getMessage());
            System.err.println(BatchArguments.usage());
            return ReturnCodes.JCL_ERROR;
        }
        ConfigurableApplicationContext context;
        try {
            context = new SpringApplicationBuilder(application).web(WebApplicationType.NONE).run(args);
        } catch (RuntimeException ex) {
            System.err.println("Service failed to start: " + ex.getMessage());
            return ReturnCodes.ABEND;
        }
        try (context) {
            BatchRun run = context.getBean(BatchJobLauncher.class).launch(arguments);
            System.out.println(new ObjectMapper().findAndRegisterModules().writeValueAsString(run.summary()));
            return run.returnCode();
        } catch (BatchJobRegistry.UnknownJobException ex) {
            System.err.println(ex.getMessage());
            return ReturnCodes.JCL_ERROR;
        } catch (Exception ex) {
            System.err.println("Job " + arguments.job() + " failed to launch: " + ex);
            return ReturnCodes.ABEND;
        }
    }
}
