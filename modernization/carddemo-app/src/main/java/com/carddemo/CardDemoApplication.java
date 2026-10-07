package com.carddemo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

/**
 * CardDemo modular monolith: one Spring Boot application hosting the online programs (CICS) and the batch
 * jobs (JCL) of the core app. Domain packages and their allowed dependencies are described in
 * {@code docs/modernization/adr/ADR-0001-modular-monolith.md} and enforced by ArchUnit.
 */
@SpringBootApplication
@ConfigurationPropertiesScan
public class CardDemoApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoApplication.class, args);
    }
}
