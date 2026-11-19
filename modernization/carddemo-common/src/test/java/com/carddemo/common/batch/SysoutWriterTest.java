package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;

import static org.assertj.core.api.Assertions.assertThat;

class SysoutWriterTest {

    @Test
    void displayConcatenatesItemsAndMasksPans(@TempDir Path out) throws Exception {
        GenerationDirectory gen = GenerationDirectory.create(out, "POSTTRAN", Clock.systemUTC());
        ByteArrayOutputStream console = new ByteArrayOutputStream();
        try (SysoutWriter sysout = SysoutWriter.open(gen, new PrintStream(console, true, StandardCharsets.UTF_8))) {
            sysout.display("START OF EXECUTION OF PROGRAM CBTRN02C");
            sysout.display("CARD NUMBER ", Pan.of("4111111111111111"), " COULD NOT BE VERIFIED");
            sysout.display("TRANSACTIONS PROCESSED :", "000000300");
        }
        String expected = """
                START OF EXECUTION OF PROGRAM CBTRN02C
                CARD NUMBER ************1111 COULD NOT BE VERIFIED
                TRANSACTIONS PROCESSED :000000300
                """;
        assertThat(Files.readString(gen.resolve(GenerationDirectory.SYSOUT))).isEqualTo(expected);
        assertThat(console.toString(StandardCharsets.UTF_8)).isEqualTo(expected);
        assertThat(Files.readString(gen.resolve(GenerationDirectory.SYSOUT))).doesNotContain("4111111111111111");
    }

    @Test
    void panRendersOnlyTheLastFour() {
        Pan pan = Pan.of("4111111111111111");
        assertThat(pan.toString()).isEqualTo("************1111");
        assertThat(String.valueOf(pan)).doesNotContain("41111");
        assertThat(pan.value()).isEqualTo("4111111111111111");
        assertThat(Pan.mask("4111111111111111  ")).isEqualTo("************1111");
        assertThat(Pan.mask("1234")).isEqualTo("****");
        assertThat(Pan.mask("")).isEmpty();
        assertThat(Pan.mask(null)).isNull();
        assertThat(Pan.of("1")).isEqualTo(Pan.of("1")).hasSameHashCodeAs(Pan.of("1"));
    }
}
