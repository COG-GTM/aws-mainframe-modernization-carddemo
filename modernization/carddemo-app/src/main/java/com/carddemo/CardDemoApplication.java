package com.carddemo;

import com.carddemo.batch.harness.BatchCommandLine;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.WebApplicationType;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

/**
 * CardDemo modular monolith: one Spring Boot application hosting the online programs (CICS) and the batch
 * jobs (JCL) of the core app. Domain packages and their allowed dependencies are described in
 * {@code docs/modernization/adr/ADR-0001-modular-monolith.md} and enforced by ArchUnit.
 *
 * <p>With {@code --job=<name>} (or {@code --spring.batch.job.name=<name>}) it runs that batch job without a web
 * server and exits with the job's JCL condition code 0/4/8/12/16 (ADR-0015); 16 also when the context fails.
 */
@SpringBootApplication
@ConfigurationPropertiesScan
public class CardDemoApplication {

    /** Exit code when the application cannot start: an abend before the job step ran. */
    static final int STARTUP_FAILURE_EXIT_CODE = 16;

    public static void main(String[] args) {
        if (!BatchCommandLine.isBatchLaunch(args)) {
            SpringApplication.run(CardDemoApplication.class, args);
            return;
        }
        System.exit(runBatch(args));
    }

    static int runBatch(String... args) {
        SpringApplication app = new SpringApplication(CardDemoApplication.class);
        app.setWebApplicationType(WebApplicationType.NONE);
        try {
            return SpringApplication.exit(app.run(args));
        } catch (RuntimeException e) {
            return STARTUP_FAILURE_EXIT_CODE;
        }
    }
}
