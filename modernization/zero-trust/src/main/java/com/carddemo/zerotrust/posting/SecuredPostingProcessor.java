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
    private final PostedTotals postedTotals;

    public SecuredPostingProcessor(ItemProcessor<DailyTransaction, PostingOutcome> delegate,
                                   WorkloadTokenSource tokenSource,
                                   SecuredTransactionPostingService secured,
                                   PostedTotals postedTotals) {
        this.delegate = delegate;
        this.tokenSource = tokenSource;
        this.secured = secured;
        this.postedTotals = postedTotals;
    }

    @Override
    public PostingOutcome process(DailyTransaction item) throws Exception {
        String recordKey = item.transaction().id();
        SecuredTransactionPostingService.VerifiedCaller caller =
                secured.authorize(tokenSource.token(), recordKey);
        PostingOutcome outcome = delegate.process(item);
        secured.auditOutcome(caller, outcome, recordKey);
        postedTotals.record(outcome);
        return outcome;
    }
}
