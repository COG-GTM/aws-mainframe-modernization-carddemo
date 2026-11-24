package com.carddemo.batch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Entry point for the CardDemo batch jobs that replace the business JCL
 * (POSTTRAN, INTCALC, TRANREPT, CREASTMT, ...).
 */
@SpringBootApplication(scanBasePackages = "com.carddemo")
public class CardDemoBatchApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(CardDemoBatchApplication.class, args)));
    }
}
