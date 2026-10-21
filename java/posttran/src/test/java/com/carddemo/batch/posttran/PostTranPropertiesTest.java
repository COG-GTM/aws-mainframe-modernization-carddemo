package com.carddemo.batch.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.posttran.seed.VsamDatasets;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

class PostTranPropertiesTest {

    @Nested
    @SpringBootTest(properties = {"posttran.run-on-startup=false", "spring.datasource.url=jdbc:h2:mem:props-default"})
    class Defaults {
        @Autowired
        private PostTranProperties properties;

        @Test
        void seedIsLoadedByDefault() {
            assertThat(properties.seed()).isEqualTo(VsamDatasets.Seed.ASCII);
            assertThat(properties.loadSeed()).isTrue();
        }
    }

    @Nested
    @SpringBootTest(properties = {"posttran.run-on-startup=false", "posttran.load-seed=false",
            "spring.datasource.url=jdbc:h2:mem:props-keep"})
    class KeepTables {
        @Autowired
        private PostTranProperties properties;

        @Test
        void loadSeedCanBeDisabled() {
            assertThat(properties.loadSeed()).isFalse();
        }
    }
}
