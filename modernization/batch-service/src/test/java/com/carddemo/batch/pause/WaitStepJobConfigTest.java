package com.carddemo.batch.pause;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.batch.BatchJobParameters;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.JobParametersBuilder;

class WaitStepJobConfigTest {

    @Test
    void readsCentisecondsFromTheFirstEightSysinColumns(@TempDir Path dir) throws Exception {
        Path sysin = Files.writeString(dir.resolve("SYSIN"), "00003600      VALUE IN CENTISECONDS\n");
        var params = new JobParametersBuilder().addString(BatchJobParameters.DD_PREFIX + "SYSIN", sysin.toString())
                .toJobParameters();
        assertThat(WaitStepJobConfig.delay(params)).isEqualTo(Duration.ofSeconds(36));
    }

    @Test
    void parmOverridesSysinAndMustBeNumeric() {
        assertThat(WaitStepJobConfig.delay(new JobParametersBuilder().addString(BatchJobParameters.PARM, "5")
                .toJobParameters())).isEqualTo(Duration.ofMillis(50));
        assertThatThrownBy(() -> WaitStepJobConfig.delay(new JobParametersBuilder()
                .addString(BatchJobParameters.PARM, "ABC").toJobParameters()))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
