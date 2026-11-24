package com.carddemo.authfrds.config;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration;
import org.springframework.boot.test.context.runner.WebApplicationContextRunner;

class ApiCredentialsTest {

    final WebApplicationContextRunner runner = new WebApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(WebMvcAutoConfiguration.class, SecurityAutoConfiguration.class))
            .withUserConfiguration(SecurityConfig.class)
            // surefire activates 'test' for the suite; this runner must start without it
            .withSystemProperties("spring.profiles.active=prod");

    @Test
    @DisplayName("BR-AF-040 startup fails fast when the API credentials are unset (no 'test' profile)")
    void failsFastWhenUnset() {
        runner.run(ctx -> {
            assertThat(ctx).hasFailed();
            assertThat(ctx.getStartupFailure()).rootCause()
                    .hasMessageContaining("AUTHFRDS_API_USER").hasMessageContaining("AUTHFRDS_API_READER_PASSWORD");
        });
    }

    @Test
    @DisplayName("BR-AF-040 startup succeeds when all four credentials are set")
    void startsWhenSet() {
        runner.withPropertyValues("carddemo.security.maintainer.username=m", "carddemo.security.maintainer.password=p1",
                        "carddemo.security.reader.username=r", "carddemo.security.reader.password=p2")
                .run(ctx -> assertThat(ctx).hasNotFailed());
    }

    @Test
    @DisplayName("BR-AF-040 maintainer and reader usernames must differ")
    void sameUserRejected() {
        assertThatThrownBy(() -> new ApiCredentials("u", "a", "u", "b").validate(false))
                .hasMessageContaining("must differ");
    }

    @Test
    @DisplayName("BR-AF-040 the unit-test 'test' profile is exempt from the fail-fast check")
    void testProfileExempt() {
        new ApiCredentials("", "", "", "").validate(true);
    }
}
