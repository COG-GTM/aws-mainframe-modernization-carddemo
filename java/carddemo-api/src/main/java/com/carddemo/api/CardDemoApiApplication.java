package com.carddemo.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Entry point for the CardDemo REST API that replaces the CICS online transactions.
 */
@SpringBootApplication(scanBasePackages = "com.carddemo")
public class CardDemoApiApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoApiApplication.class, args);
    }
}
