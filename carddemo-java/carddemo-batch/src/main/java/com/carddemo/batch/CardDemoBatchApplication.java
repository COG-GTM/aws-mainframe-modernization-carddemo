package com.carddemo.batch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@SpringBootApplication(scanBasePackages = "com.carddemo")
@EntityScan("com.carddemo.data")
@EnableJpaRepositories("com.carddemo.data")
public class CardDemoBatchApplication {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(CardDemoBatchApplication.class, args)));
    }
}
