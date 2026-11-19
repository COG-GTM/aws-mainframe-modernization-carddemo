package com.carddemo.app;

import com.carddemo.common.batch.BatchCli;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * The CardDemo modular monolith. With {@code --job=<NAME>} it is a batch launch: the context starts,
 * the job runs through {@link BatchJobLauncher} and the process exits with the job's JCL condition
 * code (0/4/12/16). Phase 0 registers no job, so every {@code --job} ends RC 12 (unknown job).
 */
@SpringBootApplication
public class CardDemoApplication {

    public static void main(String[] args) {
        if (!BatchCli.requested(args)) {
            SpringApplication.run(CardDemoApplication.class, args);
            return;
        }
        System.exit(launch(args));
    }

    static int launch(String[] args) {
        return BatchCli.run(args, arguments -> {
            try (ConfigurableApplicationContext context = new SpringApplication(CardDemoApplication.class)
                    .run(arguments.passThrough().toArray(String[]::new))) {
                return context.getBean(BatchJobLauncher.class).run(arguments);
            }
        }, System.err);
    }
}
