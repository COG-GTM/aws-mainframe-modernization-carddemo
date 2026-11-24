package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class WebResourcesTest {

    @Test
    void indexPageIsPackagedAsServletResource() {
        assertThat(getClass().getResource("/META-INF/resources/index.html")).isNotNull();
    }
}
