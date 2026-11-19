package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import static org.assertj.core.api.Assertions.assertThat;

class GenerationDirectoryTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2022-07-18T10:30:45.123Z"), ZoneOffset.UTC);

    @Test
    void createsOneDirectoryPerRunUnderTheJob(@TempDir Path out) {
        GenerationDirectory first = GenerationDirectory.create(out, "POSTTRAN", CLOCK);
        GenerationDirectory second = GenerationDirectory.create(out, "POSTTRAN", CLOCK);
        assertThat(first.path()).isEqualTo(out.resolve("POSTTRAN/20220718-103045-123"));
        assertThat(second.path()).isEqualTo(out.resolve("POSTTRAN/20220718-103045-124"));
        assertThat(GenerationDirectory.latest(out, "POSTTRAN")).contains(second.path());
        assertThat(GenerationDirectory.latest(out, "INTCALC")).isEmpty();
    }

    @Test
    void writesReturnCodeLikeTheOracle(@TempDir Path out) throws Exception {
        GenerationDirectory gen = GenerationDirectory.create(out, "POSTTRAN", CLOCK);
        gen.writeReturnCode(ReturnCode.WARNING);
        assertThat(Files.readString(gen.resolve(GenerationDirectory.RETURN_CODE))).isEqualTo("4\n");
    }

    @Test
    void datasetIsCataloguedOnlyWhenPublished(@TempDir Path out) throws Exception {
        GenerationDirectory gen = GenerationDirectory.create(out, "POSTTRAN", CLOCK);
        Files.writeString(gen.temp("DALYREJS"), "x");
        assertThat(gen.resolve("DALYREJS")).doesNotExist();
        gen.publish("DALYREJS");
        assertThat(gen.resolve("DALYREJS")).hasContent("x");
        assertThat(gen.temp("DALYREJS")).doesNotExist();

        Files.writeString(gen.temp("OTHER"), "y");
        gen.discard("OTHER");
        assertThat(gen.temp("OTHER")).doesNotExist();
        assertThat(gen.resolve("OTHER")).doesNotExist();
    }
}
