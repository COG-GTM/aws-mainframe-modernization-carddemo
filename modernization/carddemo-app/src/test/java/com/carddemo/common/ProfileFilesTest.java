package com.carddemo.common;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.boot.env.YamlPropertySourceLoader;
import org.springframework.core.env.PropertySource;
import org.springframework.core.io.ClassPathResource;

/** The runtime profiles (local, test, ci, golden) exist and never ship a database password or a JWT key. */
class ProfileFilesTest {

    private static PropertySource<?> load(String file) throws IOException {
        ClassPathResource resource = new ClassPathResource(file);
        assertThat(resource.exists()).as(file).isTrue();
        List<PropertySource<?>> sources = new YamlPropertySourceLoader().load(file, resource);
        assertThat(sources).hasSize(1);
        return sources.get(0);
    }

    @ParameterizedTest
    @CsvSource({
        "application-local.yml,  management.endpoint.health.show-details, always",
        "application-test.yml,   management.endpoint.health.show-details, always",
        "application-test.yml,   spring.flyway.clean-disabled,            true",
        "application-ci.yml,     spring.output.ansi.enabled,              never",
        "application-golden.yml, carddemo.clock.fixed,                    2022-07-06T00:00:00"
    })
    void profileSetsItsKeyProperty(String file, String key, String expected) throws IOException {
        assertThat(String.valueOf(load(file).getProperty(key))).isEqualTo(expected);
    }

    @Test
    void noProfileOverridesTheDatasourceCredentials() throws IOException {
        for (String file : List.of("application-local.yml", "application-test.yml", "application-ci.yml",
                "application-golden.yml")) {
            PropertySource<?> source = load(file);
            assertThat(source.getProperty("spring.datasource.password")).as(file).isNull();
            assertThat(source.getProperty("spring.datasource.url")).as(file).isNull();
        }
    }

    @Test
    void noProfileShipsAJwtKey() throws IOException {
        assertThat(load("application.yml").getProperty("carddemo.security.jwt.secret"))
                .isEqualTo("${CARDDEMO_JWT_SECRET:}");
        for (String file : List.of("application-local.yml", "application-test.yml", "application-ci.yml",
                "application-golden.yml")) {
            assertThat(load(file).getProperty("carddemo.security.jwt.secret")).as(file).isNull();
        }
    }

    /** Framework DEBUG/TRACE logs request paths and entity state, which can carry a full PAN (s6.4). */
    @Test
    void noProfileEnablesFrameworkDebugLogging() throws IOException {
        for (String file : List.of("application.yml", "application-local.yml", "application-test.yml",
                "application-ci.yml", "application-golden.yml")) {
            PropertySource<?> source = load(file);
            if (source instanceof org.springframework.core.env.EnumerablePropertySource<?> e) {
                for (String name : e.getPropertyNames()) {
                    if (name.startsWith("logging.level.") && !name.startsWith("logging.level.com.carddemo")) {
                        assertThat(String.valueOf(source.getProperty(name))).as(file + " " + name)
                                .isNotIn("DEBUG", "TRACE", "debug", "trace");
                    }
                }
            }
        }
    }

    @Test
    void basePasswordHasNoDefault() throws IOException {
        assertThat(load("application.yml").getProperty("spring.datasource.password"))
                .isEqualTo("${CARDDEMO_DB_PASSWORD:}");
    }
}
