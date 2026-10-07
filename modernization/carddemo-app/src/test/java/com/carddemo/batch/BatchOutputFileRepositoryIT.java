package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.support.PostgresRepositoryTest;
import java.time.LocalDate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/** GDG relative generation lookups on batch_output_file (ADR-0012). */
class BatchOutputFileRepositoryIT extends PostgresRepositoryTest {

    @Autowired
    BatchOutputFileRepository files;

    @BeforeEach
    void jobExecutions() {
        jdbc.update("insert into batch_job_instance (job_instance_id, version, job_name, job_key)"
                + " values (9001, 0, 'POSTTRAN', 'k')");
        for (long id = 9001; id <= 9003; id++) {
            jdbc.update("insert into batch_job_execution (job_execution_id, version, job_instance_id, create_time)"
                    + " values (?, 0, 9001, now())", id);
        }
    }

    BatchOutputFile save(String base, LocalDate date, long execution) {
        return files.save(new BatchOutputFile(base, date, execution, "/out/" + base + "." + date + "." + execution,
                10, "0".repeat(64)));
    }

    @Test
    void relativeGenerationsAreNewestFirstPerBase() {
        save("TRANREPT", LocalDate.of(2022, 7, 5), 9001);
        save("TRANREPT", LocalDate.of(2022, 7, 6), 9002);
        save("TRANREPT", LocalDate.of(2022, 7, 6), 9003);
        save("DALYREJS", LocalDate.of(2022, 7, 7), 9003);
        flushAndClear();

        assertThat(files.generation("TRANREPT", 0).orElseThrow().getJobExecutionId()).isEqualTo(9003);
        assertThat(files.generation("TRANREPT", -1).orElseThrow().getJobExecutionId()).isEqualTo(9002);
        BatchOutputFile oldest = files.generation("TRANREPT", -2).orElseThrow();
        assertThat(oldest.getBusinessDate()).isEqualTo(LocalDate.of(2022, 7, 5));
        assertThat(oldest.getCreatedAt()).isNotNull();
        assertThat(oldest.getOutputFileId()).isNotNull();
        assertThat(files.generation("TRANREPT", -3)).isEmpty();
        assertThat(files.generation("DALYREJS", 0).orElseThrow().getGdgBase()).isEqualTo("DALYREJS");
        assertThat(files.generation("SYSTRAN", 0)).isEmpty();
        assertThatThrownBy(() -> files.generation("TRANREPT", 1)).hasMessageContaining("(+1) is a new generation");
    }
}
