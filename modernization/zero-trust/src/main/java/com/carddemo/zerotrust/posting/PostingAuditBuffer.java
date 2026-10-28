package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.PostingOutcome;

import java.util.IdentityHashMap;
import java.util.Map;

/**
 * Carries each processed item's verified caller from the processor to the writer. Keyed on
 * outcome instance identity, not transaction id, because transaction ids can repeat. The
 * outcome is audited only once the chunk it belongs to has been written successfully.
 */
public class PostingAuditBuffer {

    private final Map<PostingOutcome, SecuredTransactionPostingService.VerifiedCaller> pending =
            new IdentityHashMap<>();

    public synchronized void put(PostingOutcome outcome,
                                 SecuredTransactionPostingService.VerifiedCaller caller) {
        pending.put(outcome, caller);
    }

    public synchronized SecuredTransactionPostingService.VerifiedCaller remove(PostingOutcome outcome) {
        return pending.remove(outcome);
    }
}
