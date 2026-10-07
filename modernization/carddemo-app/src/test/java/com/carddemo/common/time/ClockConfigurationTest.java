package com.carddemo.common.time;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

class ClockConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(ConfigurationPropertiesAutoConfiguration.class))
            .withUserConfiguration(Properties.class, ClockConfiguration.class);

    @Test
    void systemClockInUtcByDefault() {
        runner.run(ctx -> {
            Clock clock = ctx.getBean(Clock.class);
            assertThat(clock.getZone()).isEqualTo(ZoneId.of("UTC"));
            assertThat(clock.instant()).isCloseTo(Instant.now(), org.assertj.core.api.Assertions.within(
                    5, java.time.temporal.ChronoUnit.SECONDS));
        });
    }

    @Test
    void fixedClockReproducesCobCurrentDate() {
        runner.withPropertyValues("carddemo.clock.fixed=2022-07-06T00:00:00").run(ctx -> {
            Clock clock = ctx.getBean(Clock.class);
            assertThat(clock.instant()).isEqualTo(Instant.parse("2022-07-06T00:00:00Z"));
            assertThat(LocalDate.now(clock)).isEqualTo(LocalDate.of(2022, 7, 6));
        });
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties(ClockProperties.class)
    static class Properties {
    }
}
