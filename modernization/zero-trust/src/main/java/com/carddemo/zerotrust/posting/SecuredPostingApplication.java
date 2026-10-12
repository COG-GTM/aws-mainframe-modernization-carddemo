package com.carddemo.zerotrust.posting;

import com.carddemo.posting.PostingApplication;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.annotation.Import;

/**
 * The posting job under application-layer Zero Trust controls. Imports the posttran
 * application so its job configuration (steps, reader, processor, writer, close tasklet)
 * is used unchanged; this module's {@link ZeroTrustConfiguration} decorates the processor
 * and the job with token verification, authorization and audit.
 */
@SpringBootApplication
@Import(PostingApplication.class)
@ConfigurationPropertiesScan(basePackages = {"com.carddemo.zerotrust", "com.carddemo.posting"})
public class SecuredPostingApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(SecuredPostingApplication.class, args)));
    }
}
