package com.carddemo.batch;

import com.carddemo.common.batch.BatchCli;
import java.time.Clock;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

/**
 * Home of the batch programs transaction-service does not run: CBTRN03C (TRANREPT), CBSTM03A/B
 * (CREASTMT), CBEXPORT/CBIMPORT and the reader utilities. Jobs register in {@link BatchJobsConfig}.
 */
@SpringBootApplication
@ComponentScan(basePackages = {"com.carddemo.batch", "com.carddemo.common"})
public class BatchServiceApplication {

    /** Starts the web service, or with {@code --job=<JCL member>} runs that batch job once and exits with its RC. */
    public static void main(String[] args) {
        if (BatchCli.requested(args)) {
            System.exit(BatchCli.run(BatchServiceApplication.class, args));
        }
        SpringApplication.run(BatchServiceApplication.class, args);
    }

    @Bean
    public Clock clock() {
        return Clock.systemUTC();
    }
}
