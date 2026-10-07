package com.carddemo.batch.harness;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;

class JobStreamTest {

    @Test
    void stepParametersAreTheSharedOnesOverriddenByTheStepQualifiedOnes() {
        JobParameters all = new JobParametersBuilder()
                .addString("DALYTRAN", "/in/dalytran")
                .addString("SYSOUT", "/out/shared")
                .addString("STEP10.SYSOUT", "/out/step10")
                .addString("STEP15.DALYREJS", "/out/rejects")
                .addLong("run.id", 7L)
                .toJobParameters();

        JobParameters step10 = JobStream.forStep(all, "STEP10");
        assertThat(step10.getString("SYSOUT")).isEqualTo("/out/step10");
        assertThat(step10.getString("DALYTRAN")).isEqualTo("/in/dalytran");
        assertThat(step10.getString("DALYREJS")).isNull();
        assertThat(step10.getLong("run.id")).isEqualTo(7L);
        assertThat(step10.getParameters()).doesNotContainKeys("STEP10.SYSOUT", "STEP15.DALYREJS");

        JobParameters step15 = JobStream.forStep(all, "STEP15");
        assertThat(step15.getString("SYSOUT")).isEqualTo("/out/shared");
        assertThat(step15.getString("DALYREJS")).isEqualTo("/out/rejects");
    }
}
