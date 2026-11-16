package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class WebResourcesTest {

    @Test
    void packagesIndexPageAsServletResource() {
        assertThat(getClass().getResource("/META-INF/resources/index.html")).isNotNull();
    }
}
