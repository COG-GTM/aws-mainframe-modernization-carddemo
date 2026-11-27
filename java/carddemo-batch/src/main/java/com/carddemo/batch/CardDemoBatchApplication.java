package com.carddemo.batch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

/**
 * Replacement for the CardDemo JCL batch (POSTTRAN, INTCALC, TRANREPT, CREASTMT, ...). Phase 0 skeleton: jobs are
 * migrated by the batch workstream (see MIGRATION-MAP.md).
 */
@SpringBootApplication(scanBasePackages = {"com.carddemo.batch", "com.carddemo.data.mapper", "com.carddemo.data.load"})
@EntityScan("com.carddemo.data.entity")
@EnableJpaRepositories("com.carddemo.data.repository")
public class CardDemoBatchApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(CardDemoBatchApplication.class, args)));
    }
}
