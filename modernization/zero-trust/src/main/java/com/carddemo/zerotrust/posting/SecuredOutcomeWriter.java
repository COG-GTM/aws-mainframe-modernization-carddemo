package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.zerotrust.audit.AuditAction;
import com.carddemo.zerotrust.audit.HashChainedAuditLog;
import com.carddemo.zerotrust.masking.DataMasker;
import com.carddemo.zerotrust.policy.SubjectKind;
import org.springframework.batch.core.scope.context.StepSynchronizationManager;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;

import java.math.BigDecimal;

/**
 * Decorates the posttran outcome writer so outcomes are audited only after the chunk has
 * been written: each outcome's caller is taken from the {@link PostingAuditBuffer} and the
 * POSTED/REJECTED entry is appended on success. On a write failure every item in the chunk
 * is audited {@code TRANSACTION_WRITE_FAILED} / {@code NOT_WRITTEN} — the chain never claims
 * an outcome that did not reach the datasets — and the failure propagates.
 *
 * <p>Per-run totals are kept in the job ExecutionContext ({@code zt.postedCount},
 * {@code zt.postedAmount}), so concurrent executions cannot mix counters.
 */
public final class SecuredOutcomeWriter implements ItemWriter<PostingOutcome> {

    public static final String POSTED_COUNT_KEY = "zt.postedCount";
    public static final String POSTED_AMOUNT_KEY = "zt.postedAmount";

    private final ItemWriter<PostingOutcome> delegate;
    private final PostingAuditBuffer buffer;
    private final SecuredTransactionPostingService secured;
    private final HashChainedAuditLog audit;
    private final DataMasker masker;

    public SecuredOutcomeWriter(ItemWriter<PostingOutcome> delegate, PostingAuditBuffer buffer,
                                SecuredTransactionPostingService secured,
                                HashChainedAuditLog audit, DataMasker masker) {
        this.delegate = delegate;
        this.buffer = buffer;
        this.secured = secured;
        this.audit = audit;
        this.masker = masker;
    }

    @Override
    public void write(Chunk<? extends PostingOutcome> chunk) throws Exception {
        try {
            delegate.write(chunk);
        } catch (Exception e) {
            for (PostingOutcome outcome : chunk) {
                SecuredTransactionPostingService.VerifiedCaller caller = buffer.remove(outcome);
                audit.append(caller != null ? caller.workload().id() : "unknown",
                        SubjectKind.WORKLOAD, AuditAction.TRANSACTION_WRITE_FAILED,
                        "NOT_WRITTEN", outcome.transaction().id(),
                        "card=" + masker.maskPan(outcome.transaction().cardNumber())
                                + " amount=" + outcome.transaction().amount().toPlainString()
                                + " error=" + e.getClass().getSimpleName());
            }
            throw e;
        }
        long postedInChunk = 0;
        BigDecimal amountInChunk = BigDecimal.ZERO;
        for (PostingOutcome outcome : chunk) {
            SecuredTransactionPostingService.VerifiedCaller caller = buffer.remove(outcome);
            if (caller == null) {
                continue;
            }
            secured.auditOutcome(caller, outcome, outcome.transaction().id());
            if (outcome instanceof PostingOutcome.Posted p) {
                postedInChunk++;
                amountInChunk = amountInChunk.add(p.transaction().amount());
            }
        }
        var stepContext = StepSynchronizationManager.getContext();
        if (stepContext != null) {
            var ctx = stepContext.getStepExecution().getJobExecution().getExecutionContext();
            ctx.putLong(POSTED_COUNT_KEY, ctx.getLong(POSTED_COUNT_KEY, 0L) + postedInChunk);
            BigDecimal previous = ctx.containsKey(POSTED_AMOUNT_KEY)
                    ? new BigDecimal(ctx.getString(POSTED_AMOUNT_KEY)) : BigDecimal.ZERO;
            ctx.putString(POSTED_AMOUNT_KEY, previous.add(amountInChunk).toPlainString());
        }
    }
}
