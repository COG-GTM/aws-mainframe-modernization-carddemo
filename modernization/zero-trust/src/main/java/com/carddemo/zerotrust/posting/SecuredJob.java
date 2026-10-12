package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.identity.AccessTokenVerifier;
import com.carddemo.zerotrust.identity.VerifiedWorkload;
import com.carddemo.zerotrust.identity.WorkloadTokenSource;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.AccessRequest;
import com.carddemo.zerotrust.policy.Decision;
import com.carddemo.zerotrust.policy.PolicyDecisionPoint;
import com.carddemo.zerotrust.policy.Subject;
import com.carddemo.zerotrust.policy.SubjectKind;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersIncrementer;
import org.springframework.batch.core.JobParametersValidator;
import org.springframework.batch.core.repository.JobRepository;

import java.time.LocalDateTime;

/**
 * Delegating {@link Job} that brackets the real posting job with enforcement: the
 * workload's token is verified and {@code run-posting} is authorized before the steps run,
 * and the run's control totals plus the audit head are recorded when it finishes. On a
 * denial the execution is marked FAILED with an exit description and no step runs.
 */
public final class SecuredJob implements Job {

    private final Job delegate;
    private final WorkloadTokenSource tokenSource;
    private final AccessTokenVerifier verifier;
    private final PolicyDecisionPoint pdp;
    private final HashChainedAuditLog audit;
    private final PostedTotals postedTotals;
    private final DataMasker masker;
    private final JobRepository jobRepository;

    public SecuredJob(Job delegate, WorkloadTokenSource tokenSource, AccessTokenVerifier verifier,
                      PolicyDecisionPoint pdp, HashChainedAuditLog audit,
                      PostedTotals postedTotals, DataMasker masker, JobRepository jobRepository) {
        this.delegate = delegate;
        this.tokenSource = tokenSource;
        this.verifier = verifier;
        this.pdp = pdp;
        this.audit = audit;
        this.postedTotals = postedTotals;
        this.masker = masker;
        this.jobRepository = jobRepository;
    }

    @Override
    public void execute(JobExecution execution) {
        String recordKey = delegate.getName();
        VerifiedWorkload workload;
        try {
            workload = verifier.verify(tokenSource.token());
        } catch (RuntimeException e) {
            denyJob(execution, "unknown", recordKey, "token rejected: " + e.getMessage());
            return;
        }
        Decision decision = pdp.decide(AccessRequest.of(
                Subject.workload(workload.id()), SecuredTransactionPostingService.PERMISSION));
        if (!decision.allowed()) {
            denyJob(execution, workload.id(), recordKey,
                    "authz=" + decision.reason() + " jti=" + workload.jti());
            return;
        }
        postedTotals.reset();
        audit.append(workload.id(), SubjectKind.WORKLOAD, AuditAction.JOB_STARTED, "ALLOW",
                recordKey, "authz=ALLOW role=" + decision.grantingRole().orElse("")
                        + " jti=" + workload.jti());
        delegate.execute(execution);
        var ctx = execution.getExecutionContext();
        audit.append(workload.id(), SubjectKind.WORKLOAD, AuditAction.JOB_COMPLETED,
                execution.getExitStatus().getExitCode(), recordKey,
                masker.scrub("transactionCount=" + ctx.getLong("transactionCount", 0L)
                        + " rejectCount=" + ctx.getLong("rejectCount", 0L)
                        + " posted=" + postedTotals.postedCount()
                        + " postedAmount=" + postedTotals.postedAmount().toPlainString()
                        + " status=" + execution.getStatus()
                        + " exitCode=" + execution.getExitStatus().getExitCode()));
        ctx.putString("auditHeadHash", audit.head().hash());
        // The delegate already persisted the execution context before we added the
        // anchor, so push the update to the repository explicitly.
        jobRepository.updateExecutionContext(execution);
    }

    private void denyJob(JobExecution execution, String actor, String recordKey, String detail) {
        audit.append(actor, SubjectKind.WORKLOAD, AuditAction.AUTHZ_DECISION, "DENY",
                recordKey, detail);
        // The delegate's AbstractJob repository bookkeeping never runs, so the FAILED
        // state and timestamps must be persisted here — otherwise the repository keeps
        // the execution as STARTING and a later launch hits JobExecutionAlreadyRunning.
        if (execution.getStartTime() == null) {
            execution.setStartTime(LocalDateTime.now());
        }
        execution.setEndTime(LocalDateTime.now());
        execution.setStatus(BatchStatus.FAILED);
        execution.setExitStatus(new ExitStatus("FAILED", "zero trust enforcement: " + detail));
        jobRepository.update(execution);
    }

    @Override
    public String getName() {
        return delegate.getName();
    }

    @Override
    public boolean isRestartable() {
        return delegate.isRestartable();
    }

    @Override
    public JobParametersIncrementer getJobParametersIncrementer() {
        return delegate.getJobParametersIncrementer();
    }

    @Override
    public JobParametersValidator getJobParametersValidator() {
        return delegate.getJobParametersValidator();
    }
}
