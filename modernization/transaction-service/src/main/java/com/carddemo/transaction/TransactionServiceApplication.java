package com.carddemo.transaction;

import com.carddemo.common.batch.BatchCli;
import java.time.Clock;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan(basePackages = {"com.carddemo.transaction", "com.carddemo.common"})
public class TransactionServiceApplication {

    /** Starts the web service, or with {@code --job=<JCL member>} runs that batch job once and exits with its RC. */
    public static void main(String[] args) {
        if (BatchCli.requested(args)) {
            System.exit(BatchCli.run(TransactionServiceApplication.class, args));
        }
        SpringApplication.run(TransactionServiceApplication.class, args);
    }

    @Bean
    public Clock clock() {
        return Clock.systemUTC();
    }
}
