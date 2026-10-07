package com.carddemo.common.time;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.BaselineRunProperties;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ActiveProfiles;

/** The {@code golden} profile pins exactly the values of {@code scripts/baseline/README.md} (ADR-0014). */
@SpringBootTest(classes = {GoldenProfileTest.Properties.class, ClockConfiguration.class})
@ActiveProfiles("golden")
class GoldenProfileTest {

    @Autowired
    Clock clock;

    @Autowired
    BaselineRunProperties baseline;

    @Test
    void clockIsPinnedToCobCurrentDate() {
        assertThat(LocalDateTime.now(clock)).isEqualTo(LocalDateTime.of(2022, 7, 6, 0, 0));
    }

    @Test
    void jclParametersMatchTheBaselineRun() {
        assertThat(baseline.intcalcParmDate()).isEqualTo("2022071800");
        assertThat(baseline.tranreptStartDate()).isEqualTo(LocalDate.of(2022, 1, 1));
        assertThat(baseline.tranreptEndDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(baseline.waitstepCentiseconds()).isEqualTo(3600);
    }

    @Configuration(proxyBeanMethods = false)
    @EnableConfigurationProperties({ClockProperties.class, BaselineRunProperties.class})
    static class Properties {
    }
}
