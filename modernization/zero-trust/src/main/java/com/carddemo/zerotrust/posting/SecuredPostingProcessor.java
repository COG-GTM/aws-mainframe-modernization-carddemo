package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostingOutcome;
import com.carddemo.zerotrust.identity.WorkloadTokenSource;
import org.springframework.batch.item.ItemProcessor;

/**
 * Batch-facing processor: obtains the workload's current access token (refreshed near
 * expiry by the token source), then runs the secured token-verify → authorize → delegate →
 * audit flow for every item. The delegate is the original posting processor, so posttran's
 * posting logic runs unchanged once the call is authorized.
 */
public final class SecuredPostingProcessor implements ItemProcessor<DailyTransaction, PostingOutcome> {

    private final ItemProcessor<DailyTransaction, PostingOutcome> delegate;
    private final WorkloadTokenSource tokenSource;
    private final SecuredTransactionPostingService secured;
    private final PostingAuditBuffer buffer;

    public SecuredPostingProcessor(ItemProcessor<DailyTransaction, PostingOutcome> delegate,
                                   WorkloadTokenSource tokenSource,
                                   SecuredTransactionPostingService secured,
                                   PostingAuditBuffer buffer) {
        this.delegate = delegate;
        this.tokenSource = tokenSource;
        this.secured = secured;
        this.buffer = buffer;
    }

    @Override
    public PostingOutcome process(DailyTransaction item) throws Exception {
        // Denials are audited and thrown here; outcomes are not audited until the chunk
        // has actually been written (see SecuredOutcomeWriter).
        SecuredTransactionPostingService.VerifiedCaller caller =
                secured.authorize(tokenSource.token(), item.transaction().id());
        PostingOutcome outcome = delegate.process(item);
        buffer.put(outcome, caller);
        return outcome;
    }
}
