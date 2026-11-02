package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;

import static org.assertj.core.api.Assertions.assertThat;

class JobRunDirectoryTest {

    @TempDir
    Path root;

    @Test
    void eachRunIsANewTimestampedGeneration() throws Exception {
        Instant now = Instant.parse("2022-07-18T10:15:30.123Z");
        JobRunDirectory first = JobRunDirectory.create(root, "POSTTRAN", now);
        JobRunDirectory second = JobRunDirectory.create(root, "POSTTRAN", now);

        assertThat(first.path()).isEqualTo(root.resolve("POSTTRAN/20220718-101530-123"));
        assertThat(second.path()).isEqualTo(root.resolve("POSTTRAN/20220718-101530-123-1"));
        first.writeReturnCode(4);
        assertThat(Files.readString(first.dd("RETURN-CODE"))).isEqualTo("4\n");
    }

    @Test
    void latestGenerationIsTheNewestThatHasTheDd() throws Exception {
        JobRunDirectory older = JobRunDirectory.create(root, "INTCALC", Instant.parse("2022-07-01T00:00:00Z"));
        JobRunDirectory newer = JobRunDirectory.create(root, "INTCALC", Instant.parse("2022-08-01T00:00:00Z"));
        JobRunDirectory newestWithoutDd = JobRunDirectory.create(root, "INTCALC", Instant.parse("2022-09-01T00:00:00Z"));
        Files.writeString(older.dd("SYSTRAN"), "old");
        Files.writeString(newer.dd("SYSTRAN"), "new");

        assertThat(newestWithoutDd.path()).exists();
        assertThat(JobRunDirectory.latestGeneration(root, "INTCALC", "SYSTRAN")).contains(newer.dd("SYSTRAN"));
        assertThat(JobRunDirectory.latestGeneration(root, "TRANBKP", "TRANSACT.BKUP")).isEmpty();
    }
}
