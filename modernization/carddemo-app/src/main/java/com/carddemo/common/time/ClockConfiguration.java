package com.carddemo.common.time;

import java.time.Clock;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Provides the single {@link Clock} every component must use instead of {@code LocalDate.now()} so baseline
 * runs can be pinned to the GnuCOBOL clock (ADR-0014).
 */
@Configuration(proxyBeanMethods = false)
public class ClockConfiguration {

    @Bean
    Clock businessClock(ClockProperties properties) {
        if (properties.fixed() == null) {
            return Clock.system(properties.zone());
        }
        return Clock.fixed(properties.fixed().atZone(properties.zone()).toInstant(), properties.zone());
    }
}
