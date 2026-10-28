package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.identity.AccessTokenVerifier;
import com.carddemo.zerotrust.identity.VerifiedWorkload;
import com.carddemo.zerotrust.identity.WorkloadTokenSource;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.PolicyLoader;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mockito;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersIncrementer;
import org.springframework.batch.core.JobParametersValidator;
import org.springframework.batch.core.repository.JobRepository;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * When audit finalization fails after the delegate committed a terminal status, the job
 * is persisted FAILED (AbstractJob contract) rather than left COMPLETED and unanchored.
 */
class SecuredJobFinalizationTest {

    @TempDir
    Path dir;

    @Test
    void auditFailureAfterCompletionMarksJobFailed() throws Exception {
        Path log = dir.resolve("audit.jsonl");
        HashChainedAuditLog audit = new HashChainedAuditLog(log, Clock.systemUTC());
        AccessTokenVerifier verifier = Mockito.mock(AccessTokenVerifier.class);
        Mockito.when(verifier.verify(Mockito.anyString()))
                .thenReturn(new VerifiedWorkload("svc-posttran-post", "jti-1", Instant.now().plusSeconds(60)));
        WorkloadTokenSource tokens = Mockito.mock(WorkloadTokenSource.class);
        Mockito.when(tokens.token()).thenReturn("token");
        PolicyDecisionPoint pdp = new PolicyDecisionPoint(PolicyLoader.load(
                getClass().getResourceAsStream("/zero-trust-policy.yaml")));
        JobRepository repository = Mockito.mock(JobRepository.class);

        Job delegate = new Job() {
            @Override public String getName() { return "dailyTransactionPostingJob"; }
            @Override public void execute(JobExecution execution) {
                execution.setStatus(BatchStatus.COMPLETED);
                // Break the audit log mid-flight: appends now throw.
                try { audit.close(); } catch (IOException ignored) { }
            }
            @Override public boolean isRestartable() { return false; }
            @Override public JobParametersIncrementer getJobParametersIncrementer() { return null; }
            @Override public JobParametersValidator getJobParametersValidator() { return null; }
        };

        SecuredJob job = new SecuredJob(delegate, tokens, verifier, pdp, audit,
                new DataMasker(), repository);
        JobExecution execution = new JobExecution(1L);
        job.execute(execution);

        assertThat(execution.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(execution.getExitStatus().getExitDescription())
                .contains("audit finalization failed");
        assertThat(execution.getEndTime()).isNotNull();
        assertThat(execution.getFailureExceptions()).isNotEmpty();
        Mockito.verify(repository).update(execution);
    }
}
