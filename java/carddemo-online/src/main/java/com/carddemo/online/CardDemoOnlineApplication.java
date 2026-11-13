package com.carddemo.online;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

/**
 * Replacement for the CICS online region (transactions CC00, CM00, CA00, ...). Phase 0 skeleton: screens are
 * migrated program by program by the online workstream (see MIGRATION-MAP.md).
 */
@SpringBootApplication(scanBasePackages = {"com.carddemo.online", "com.carddemo.data.mapper", "com.carddemo.data.load"})
@EntityScan("com.carddemo.data.entity")
@EnableJpaRepositories("com.carddemo.data.repository")
public class CardDemoOnlineApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoOnlineApplication.class, args);
    }
}
