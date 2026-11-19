package com.carddemo.app;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchCli;
import com.carddemo.common.batch.JclJobLauncher;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

import java.io.PrintStream;

/**
 * The CardDemo modular monolith. With {@code --job=<JCL member>} it is a batch launch (D-014): the context
 * starts, the job runs through {@link JclJobLauncher}, the context closes and the process exits with the
 * job's JCL condition code (D-015).
 */
@SpringBootApplication(scanBasePackages = "com.carddemo")
public class CardDemoApplication {

    public static void main(String[] args) {
        if (!BatchArguments.isBatchLaunch(args)) {
            SpringApplication.run(CardDemoApplication.class, args);
            return;
        }
        System.exit(launch(args, System.err));
    }

    static int launch(String[] args, PrintStream err) {
        return BatchCli.run(args, arguments -> {
            try (ConfigurableApplicationContext context = new SpringApplication(CardDemoApplication.class)
                    .run(arguments.passThrough().toArray(String[]::new))) {
                return context.getBean(JclJobLauncher.class).run(arguments);
            }
        }, err);
    }
}
