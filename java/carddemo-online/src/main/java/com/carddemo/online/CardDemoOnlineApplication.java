package com.carddemo.online;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Replacement for the CICS online region (transactions CC00, CM00, CA00, ...). Phase 0 skeleton: screens are
 * migrated program by program by the online workstream (see MIGRATION-MAP.md).
 */
@SpringBootApplication(scanBasePackages = {"com.carddemo.online", "com.carddemo.data"})
public class CardDemoOnlineApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoOnlineApplication.class, args);
    }
}
