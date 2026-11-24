package com.carddemo.api;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages = "com.carddemo")
public class CardDemoApiApplication {

    public static void main(String[] args) {
        SpringApplication.run(CardDemoApiApplication.class, args);
    }
}
