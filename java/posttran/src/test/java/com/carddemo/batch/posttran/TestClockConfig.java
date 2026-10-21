package com.carddemo.batch.posttran;

import java.time.Clock;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

@TestConfiguration
public class TestClockConfig {

    @Bean
    @Primary
    Clock fixedClock() {
        return TestData.FIXED_CLOCK;
    }
}
