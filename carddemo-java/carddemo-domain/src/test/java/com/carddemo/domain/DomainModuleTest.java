package com.carddemo.domain;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class DomainModuleTest {

    @Test
    void domainHasNoFrameworkOnTheClasspath() {
        assertThat(isPresent("org.springframework.context.ApplicationContext")).isFalse();
        assertThat(isPresent("jakarta.persistence.Entity")).isFalse();
    }

    private static boolean isPresent(String className) {
        try {
            Class.forName(className, false, DomainModuleTest.class.getClassLoader());
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
    }
}
