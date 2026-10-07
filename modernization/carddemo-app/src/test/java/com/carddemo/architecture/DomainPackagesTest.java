package com.carddemo.architecture;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/** Every package named in ADR-0001 exists (as a documented {@code package-info}) even before it has code. */
class DomainPackagesTest {

    static Stream<String> packages() {
        return Stream.concat(Stream.of(ModularMonolithRules.COMMON),
                ModularMonolithRules.ALLOWED_DOMAIN_DEPENDENCIES.keySet().stream()).sorted();
    }

    @ParameterizedTest
    @MethodSource("packages")
    void packageIsDeclared(String name) {
        assertThat(getClass().getResource("/com/carddemo/" + name + "/package-info.class"))
                .as("com.carddemo.%s/package-info.class", name)
                .isNotNull();
    }

    @org.junit.jupiter.api.Test
    void matrixCoversTheSixDomainsOfTheDecomposition() {
        assertThat(ModularMonolithRules.ALLOWED_DOMAIN_DEPENDENCIES)
                .containsOnlyKeys("customer", "account", "card", "transaction", "user", "batch");
    }
}
