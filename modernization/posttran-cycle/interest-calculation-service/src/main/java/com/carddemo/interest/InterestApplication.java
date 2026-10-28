package com.carddemo.interest;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

/** Interest calculation microservice: Java replacement for CBACT04C (INTCALC.jcl STEP15). */
@SpringBootApplication
@ConfigurationPropertiesScan
public class InterestApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(InterestApplication.class, args)));
    }
}
