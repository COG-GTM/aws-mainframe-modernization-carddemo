package com.carddemo.batch.posttran;

import java.time.Clock;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
@EnableConfigurationProperties(PostTranProperties.class)
public class PostTranApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(PostTranApplication.class, args)));
    }

    /** Source of FUNCTION CURRENT-DATE. */
    @Bean
    Clock clock() {
        return Clock.systemDefaultZone();
    }
}
