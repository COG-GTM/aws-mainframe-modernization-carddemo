package com.carddemo.online;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/** CardDemo online region: CICS transactions exposed as REST endpoints. */
@SpringBootApplication
public class CarddemoOnlineApplication {

    public static void main(String[] args) {
        SpringApplication.run(CarddemoOnlineApplication.class, args);
    }
}
