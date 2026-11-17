package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.mock.env.MockEnvironment;

class SampleDataLoadGuardTest {

    @Test
    void refusedWithoutDevOrTestProfileOrOptIn() {
        assertThat(SampleDataLoadJobConfiguration.isEnabled(new MockEnvironment())).isFalse();
        MockEnvironment prod = new MockEnvironment();
        prod.setActiveProfiles("prod");
        assertThat(SampleDataLoadJobConfiguration.isEnabled(prod)).isFalse();
        assertThat(SampleDataLoadJobConfiguration.isEnabled(
                new MockEnvironment().withProperty(SampleDataLoadJobConfiguration.ENABLED_PROPERTY, "false"))).isFalse();
    }

    @Test
    void allowedUnderDevOrTestProfileOrOptIn() {
        for (String profile : new String[] {"dev", "test"}) {
            MockEnvironment environment = new MockEnvironment();
            environment.setActiveProfiles(profile);
            assertThat(SampleDataLoadJobConfiguration.isEnabled(environment)).isTrue();
        }
        assertThat(SampleDataLoadJobConfiguration.isEnabled(
                new MockEnvironment().withProperty(SampleDataLoadJobConfiguration.ENABLED_PROPERTY, "true"))).isTrue();
    }
}
