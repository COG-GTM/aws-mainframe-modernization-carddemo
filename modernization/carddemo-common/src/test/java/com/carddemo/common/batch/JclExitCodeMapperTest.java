package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class JclExitCodeMapperTest {

    @Test
    void completedWithoutRcIsZero() {
        JobExecution execution = execution(BatchStatus.COMPLETED);
        assertThat(JclExitCodeMapper.map(execution)).isZero();
    }

    @Test
    void completedKeepsHighestRaisedRc() {
        JobExecution execution = execution(BatchStatus.COMPLETED);
        JclExitCodeMapper.raise(execution, 4);
        JclExitCodeMapper.raise(execution, 0);
        assertThat(JclExitCodeMapper.map(execution)).isEqualTo(ReturnCode.WARNING);
        assertThatThrownBy(() -> JclExitCodeMapper.raise(execution, 4096)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void anyOtherEndStateIsAbend16() {
        for (BatchStatus status : new BatchStatus[] {BatchStatus.FAILED, BatchStatus.STOPPED, BatchStatus.ABANDONED, BatchStatus.UNKNOWN}) {
            JobExecution execution = execution(status);
            JclExitCodeMapper.raise(execution, 4);
            assertThat(JclExitCodeMapper.map(execution)).as(status.name()).isEqualTo(ReturnCode.ABEND);
        }
    }

    private static JobExecution execution(BatchStatus status) {
        JobExecution execution = new JobExecution(1L);
        execution.setStatus(status);
        return execution;
    }
}
