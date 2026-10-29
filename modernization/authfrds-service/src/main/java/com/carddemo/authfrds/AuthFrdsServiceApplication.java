package com.carddemo.authfrds;

import java.time.Clock;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class AuthFrdsServiceApplication {

    public static void main(String[] args) {
        SpringApplication.run(AuthFrdsServiceApplication.class, args);
    }

    /** CICS ASKTIME equivalent. */
    @Bean
    Clock clock() {
        return Clock.systemDefaultZone();
    }
}
