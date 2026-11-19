package com.carddemo.app;

import com.carddemo.common.batch.BatchCli;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.AutoConfigurationPackage;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

/**
 * The CardDemo modular monolith. With {@code --job=<NAME>} it is a batch launch: the context starts,
 * the job runs through {@link BatchJobLauncher} and the process exits with the job's JCL condition
 * code (0/4/12/16). Registered jobs: {@code LOAD} (seed tables, {@link DataLoader}); POSTTRAN arrives in Wave 1.
 * Bounded contexts live in {@code com.carddemo.<context>} (target state), so scanning starts at {@code com.carddemo}.
 */
@SpringBootApplication(scanBasePackages = "com.carddemo")
@AutoConfigurationPackage(basePackages = "com.carddemo")
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
