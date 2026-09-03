package com.carddemo.report;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

/** Daily transaction report microservice: Java replacement for TRANREPT.jcl (SORT step + CBTRN03C). */
@SpringBootApplication
@ConfigurationPropertiesScan
public class ReportApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(ReportApplication.class, args)));
    }
}
