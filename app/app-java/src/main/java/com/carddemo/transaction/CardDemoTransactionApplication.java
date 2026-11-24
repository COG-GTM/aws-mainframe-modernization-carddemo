package com.carddemo.transaction;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
@ConfigurationPropertiesScan
public class CardDemoTransactionApplication {

    public static void main(String[] args) {
        ConfigurableApplicationContext context = SpringApplication.run(CardDemoTransactionApplication.class, args);
        boolean batchMode = context.getEnvironment().getProperty("spring.batch.job.enabled", Boolean.class, false);
        if (batchMode) {
            // Batch runs end like a JCL step: the JVM exit code carries the COBOL RETURN-CODE.
            System.exit(SpringApplication.exit(context));
        }
    }
}
