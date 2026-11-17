package com.carddemo.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@SpringBootApplication(scanBasePackages = "com.carddemo")
@EntityScan("com.carddemo.data")
@EnableJpaRepositories("com.carddemo.data")
public class CardDemoApiApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoApiApplication.class, args);
    }
}
