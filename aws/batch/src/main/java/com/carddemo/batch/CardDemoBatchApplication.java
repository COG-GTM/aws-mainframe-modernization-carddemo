package com.carddemo.batch;

import com.carddemo.batch.core.JobRunner;
import com.carddemo.batch.core.ReturnCode;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.context.ConfigurableApplicationContext;

/** {@code java -jar carddemo-batch.jar --job=<job-name> [--runId=…] [--businessDate=yyyy-MM-dd] [params]}. */
@SpringBootApplication(exclude = BatchAutoConfiguration.class)
public class CardDemoBatchApplication {

    public static void main(String[] args) {
        ConfigurableApplicationContext ctx = SpringApplication.run(CardDemoBatchApplication.class, args);
        int returnCode = ctx.getBean(JobRunner.class).run(args);
        int exitCode = ReturnCode.processExitCode(returnCode);
        System.exit(SpringApplication.exit(ctx, () -> exitCode));
    }
}
