package com.carddemo.batch.harness;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.AbendException;
import com.carddemo.common.codec.RecordFormatException;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobInstance;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;

class ReturnCodeTest {

    @Test
    void jclConditionCodes() {
        assertThat(ReturnCode.values()).extracting(ReturnCode::code).containsExactly(0, 4, 8, 12, 16);
        assertThat(ReturnCode.of(4)).isEqualTo(ReturnCode.WARNING);
        assertThat(ReturnCode.WARNING.isFailure()).isFalse();
        assertThat(ReturnCode.ERROR.isFailure()).isTrue();
        assertThat(ReturnCode.WARNING.max(ReturnCode.SEVERE)).isEqualTo(ReturnCode.SEVERE);
        assertThat(ReturnCode.TERMINAL.max(ReturnCode.OK)).isEqualTo(ReturnCode.TERMINAL);
        assertThat(ReturnCode.WARNING.label()).isEqualTo("RC=0004");
        assertThatThrownBy(() -> ReturnCode.of(5)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> new ReturnCodeException(ReturnCode.WARNING, "x"))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void failuresAreClassifiedByTheirMostSpecificCause() {
        assertThat(ReturnCode.classify(new RuntimeException("boom"))).isEqualTo(ReturnCode.SEVERE);
        assertThat(ReturnCode.classify(new IllegalStateException(new RecordFormatException("bad"))))
                .isEqualTo(ReturnCode.ERROR);
        assertThat(ReturnCode.classify(new RuntimeException(new ReturnCodeException(ReturnCode.ERROR, "x"))))
                .isEqualTo(ReturnCode.ERROR);
        assertThat(ReturnCode.classify(new RuntimeException(new AbendException(999, "abend"))))
                .isEqualTo(ReturnCode.TERMINAL);
        assertThat(ReturnCode.isAbend(List.of(new AbendException(999, "abend")))).isTrue();
        assertThat(ReturnCode.isAbend(List.of(new RuntimeException()))).isFalse();
    }

    @Test
    void jobRcIsTheHighestStepRc() {
        JobExecution job = new JobExecution(new JobInstance(1L, "j"), 1L, new JobParameters());
        StepExecution warn = job.createStepExecution("STEP01");
        warn.setStatus(BatchStatus.COMPLETED);
        ReturnCode.set(warn, ReturnCode.WARNING);
        ReturnCode.set(warn, ReturnCode.OK);
        assertThat(ReturnCode.of(warn)).isEqualTo(ReturnCode.WARNING);
        StepExecution ok = job.createStepExecution("STEP02");
        ok.setStatus(BatchStatus.COMPLETED);
        job.setStatus(BatchStatus.COMPLETED);
        assertThat(ReturnCode.of(job)).isEqualTo(ReturnCode.WARNING);

        StepExecution failed = job.createStepExecution("STEP03");
        failed.setStatus(BatchStatus.FAILED);
        failed.addFailureException(new ReturnCodeException(ReturnCode.SEVERE, "x"));
        job.setStatus(BatchStatus.FAILED);
        assertThat(ReturnCode.of(failed)).isEqualTo(ReturnCode.SEVERE);
        assertThat(ReturnCode.of(job)).isEqualTo(ReturnCode.SEVERE);

        job.setStatus(BatchStatus.STOPPED);
        assertThat(ReturnCode.of(job)).isEqualTo(ReturnCode.TERMINAL);
    }

    @Test
    void aFailedJobWithoutAClassifiableCauseIsAtLeastAnError() {
        JobExecution job = new JobExecution(new JobInstance(1L, "j"), 1L, new JobParameters());
        job.setStatus(BatchStatus.FAILED);
        assertThat(ReturnCode.of(job)).isEqualTo(ReturnCode.ERROR);
    }
}
