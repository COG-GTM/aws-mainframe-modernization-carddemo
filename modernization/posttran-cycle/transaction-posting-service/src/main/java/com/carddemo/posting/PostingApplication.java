package com.carddemo.posting;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

/**
 * Daily transaction posting microservice: the Java replacement for {@code CBTRN02C} as run by
 * {@code app/jcl/POSTTRAN.jcl STEP15}. Runs one Spring Batch job and exits with the RETURN-CODE the
 * COBOL program would have set (0 = clean, 4 = rejects written).
 */
@SpringBootApplication
@ConfigurationPropertiesScan
public class PostingApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(PostingApplication.class, args)));
    }
}
