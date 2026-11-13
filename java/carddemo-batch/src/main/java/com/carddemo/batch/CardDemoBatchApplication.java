package com.carddemo.batch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Replacement for the CardDemo JCL batch (POSTTRAN, INTCALC, TRANREPT, CREASTMT, ...). Phase 0 skeleton: jobs are
 * migrated by the batch workstream (see MIGRATION-MAP.md).
 */
@SpringBootApplication(scanBasePackages = {"com.carddemo.batch", "com.carddemo.data"})
public class CardDemoBatchApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(CardDemoBatchApplication.class, args)));
    }
}
